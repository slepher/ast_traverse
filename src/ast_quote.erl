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
-export([quote/1, quote/2]).

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

quote(Value) ->
    quote(Value, 0).

quote({call, _Line1, {atom, _Line2, unquote}, [Unquote]}, _Line) ->
    Unquote;
quote({call, Line1, {atom, _Line2, unquote_splicing}, Unquotes}, _Line) ->
    {block, Line1, Unquotes};
quote(Tuple, Line) when is_tuple(Tuple) ->
    TupleList = tuple_to_list(Tuple),
    QuotedLine = 
        case TupleList of
            [_Action, TupleLine|_Rest] ->
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
walk(_Type, Node) ->
    Node.
