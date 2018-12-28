%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 19 Oct 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(ast_quote).

%% API
-export([parse_transform/2, format_error/1]).
-export([abstract/2, quote/1, quote/2, replace_line/2, replace_line_zero/2, merge_clauses/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Options) ->
    ast_traverse:map(fun walk/2, Forms).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.

abstract(Term, Line) ->
    erl_syntax:set_pos(erl_syntax:abstract(Term), Line).

replace_line(Ast, Line) ->
    replace_line_cond(fun(_) -> true end, Ast, Line).

replace_line_zero(Ast, Line) ->
    replace_line_cond(
      fun(0) -> true;
         (_) -> false
      end, Ast, Line).

replace_line_cond(Cond, Ast, Line) ->
    ast_traverse:map(
      fun(Type, Tuple) when is_tuple(Tuple) and ((Type == pre) or (Type == leaf)) ->
              case tuple_to_list(Tuple) of
                  [_Action, TupleLine|_Rest] when is_integer(TupleLine) ->
                      case Cond(TupleLine) of
                          true ->
                              setelement(2, Tuple, Line);
                          false ->
                              Tuple
                      end;
                  _ ->
                      Tuple
              end;
         (_, Node) ->
              Node
         end, Ast).

merge_clauses([{'fun', Line, {clauses, _}}|_T] = Nodes) -> 
    NClauses = 
        lists:flatten(
          lists:map(
            fun({'fun', _, {clauses, FClauses}}) ->
                    FClauses
            end, Nodes)),
    {'fun', Line, {clauses, NClauses}}.


quote(Value) ->
    quote_1(Value, #{line => 0}).

quote(Value, Line) when is_integer(Line) ->
    quote_1(Value, #{line => Line});
quote(Value, Opts) when is_map(Opts) ->
    quote_1(Value, Opts).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
walk(pre, {call, _Line1, {atom, Line2, quote}, [Form]}) ->
    quote(Form, Line2);
walk(pre, {call, _Line1, {atom, Line2, quote}, [Form, Line]}) ->
    {call, Line2, {remote, Line2, {atom, Line2, ast_quote}, {atom, Line2, replace_line_zero}},
     [quote(Form, #{line => Line2, replaced_line => true}), Line]};
walk(_Type, Node) ->
    Node.

quote_1({call, _Line1, {atom, _Line2, unquote}, [Unquote]}, _Opts) ->
    Unquote;
quote_1([{call, _Line1, {atom, _Line2, unquote_splicing}, [Unquotes]}], _Opts) ->
    Unquotes;
quote_1({match, _, {atom, _, unquote}, Unquote}, _Opts) ->
    Unquote;
quote_1([{match, _, {atom, _, unquote_splicing}, Unquotes}], _Opts) ->
    Unquotes;
quote_1([{atom, _, unquote_splicing}|Unquotes], Opts) ->
    quote_list(Unquotes, Opts);
quote_1(Tuple, Opts) when is_tuple(Tuple) ->
    quote_tuple(Tuple, Opts);
quote_1([H|T], #{line := Line} = Opts) ->
    {cons, Line, quote_1(H, Opts), quote_1(T, Opts)};
quote_1([], #{line := Line}) ->
    {nil, Line};
quote_1(Float, #{line := Line}) when is_float(Float) ->
    {float, Line, Float};
quote_1(Integer, #{line := Line}) when is_integer(Integer) ->
    {integer, Line, Integer};
quote_1(Atom, #{line := Line}) when is_atom(Atom) ->
    {atom, Line, Atom}.

quote_list([H|T], #{line := Line} = Opts) ->
    {cons, Line, H, quote_list(T, Opts)};
quote_list([], #{line := Line}) ->
    {nil, Line}.

quote_tuple(Tuple, #{line := Line} = Opts) ->
    TupleList = tuple_to_list(Tuple),
    case TupleList of
        [_Action, TupleLine|_Rest] when is_integer(TupleLine) ->
            NOpts = Opts#{line => TupleLine},
            {tuple, TupleLine, quote_tuple_list(TupleList, NOpts)};
        _ ->
            {tuple, Line, quote_tuple_list_1(TupleList, Opts)}
    end.

quote_tuple_list([Action, _TupleLine|Rest], #{replaced_line := true} = Opts) ->
    quote_tuple_list_1([Action, 0|Rest], Opts);
quote_tuple_list(TupleList, Opts) ->
    quote_tuple_list_1(TupleList, Opts).

quote_tuple_list_1(List, Opts) ->
    lists:map(fun(Item) -> quote_1(Item, Opts) end, List).
