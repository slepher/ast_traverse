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
-export([quote/1, quote/2, replace_line/2, merge_clauses/1]).

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

replace_line(Ast, Line) ->
    ast_traverse:map(
      fun(Type, Tuple) when is_tuple(Tuple) and ((Type == pre) or (Type == leaf)) ->
              TupleList = tuple_to_list(Tuple),
              case TupleList of
                  [_Action, TupleLine|_Rest] when is_integer(TupleLine) ->
                      setelement(2, Tuple, Line);
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
    quote(Value, 0).

quote({call, _Line1, {atom, _Line2, unquote}, [Unquote]}, _Line) ->
    Unquote;
quote([{call, _Line1, {atom, _Line2, unquote_splicing}, [Unquotes]}], _Line) ->
    Unquotes;
quote([{atom, _, unquote_splicing}|Unquotes], Line) ->
    quote_list(Unquotes, Line);
quote(Tuple, Line) when is_tuple(Tuple) ->
    TupleList = tuple_to_list(Tuple),
    QuotedLine = 
        case TupleList of
            [_Action, TupleLine|_Rest] when is_integer(TupleLine) ->
                TupleLine;
            _ ->
                Line
        end,
    {tuple, QuotedLine, lists:map(fun(Item) -> quote(Item, QuotedLine) end, TupleList)};
quote([H|T], Line) ->
    {cons, Line, quote(H, Line), quote(T, Line)};
quote([], Line) ->
    {nil, Line};
quote(Float, Line) when is_float(Float) ->
    {float, Line, Float};
quote(Integer, Line) when is_integer(Integer) ->
    {integer, Line, Integer};
quote(Atom, Line) when is_atom(Atom) ->
    {atom, Line, Atom}.

quote_list([H|T], Line) ->
    {cons, Line, H, quote_list(T, Line)};
quote_list([], Line) ->
    {nil, Line}.
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
walk(pre, {call, _Line1, {atom, Line2, quote}, [Form]}) ->
    Node = quote(Form, Line2),
    Node;
walk(pre, {call, Line1, {atom, Line2, quote}, [Form, Line]}) ->
    {call, Line1, {remote, Line2, {atom, Line2, ast_quote}, {atom, Line2, replace_line}}, [quote(Form, Line2), Line]};
walk(_Type, Node) ->
    Node.
