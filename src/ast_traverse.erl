%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 18 Sep 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(ast_traverse).

%% API
-export([map_with_state/3, map/2, reduce/3]).
-export([mapfold/3]).
-export([map_m/3]).
-export([attributes/2, attributes_with_line/2, module_attributes/2, from_value/2, read/1]).

%%%===================================================================
%%% API
%%%===================================================================

-spec map_with_state(fun((_Type, Node, State) -> {Node, State}), State, Node) -> Node.
map_with_state(F, Init, Forms) ->
    {NForms, _State} = mapfold(F, Init, Forms),
    NForms.

-spec map(fun((_Type, Node) -> Node), Node) -> Node.
map(F, TopNode) ->
    map_with_state(fun(Type, Node, State) -> {F(Type, Node), State} end, ok, TopNode).

-spec reduce(fun((_Type, Node, State) -> State), State, Node) -> State.
reduce(F, Init, TopNode) ->
    {_NForms, NState} = 
        mapfold(fun(Type, Node, State) -> {Node, F(Type, Node, State)} end, Init, TopNode),
    NState.

-spec mapfold(fun((_Type, Node, State) -> {Node, State}), State, Node) -> {Node, State}.
mapfold(F, Init, TopNode) ->
    STNode = map_m(ast_state, 
                   fun(Type, Node) -> fun(State) -> F(Type, Node, State) end end, TopNode),
    ast_state:run(STNode, Init).

-spec map_m(M, fun((_Type, Node) -> ast_monad:monadic(M, Node)), Node) -> 
                   ast_monad:monadic(M, Node) when M :: ast_monad:monad().
map_m(Monad, F, Nodes) when is_list(Nodes) ->
    ast_monad:map_m(
      Monad,
      fun(Subtree) ->
              map_m(Monad, F, Subtree)
      end, Nodes);
map_m(Monad, F, XNode) ->
    PreType = 
        case erl_syntax:subtrees(XNode) of
            [] ->
                leaf;
            _Subtrees ->
                pre
        end,
    %% do form
    %% do([Monad ||
    %%           YNode <- F(pre, XNode),
    %%           NSubtrees <- map_m(Monad, F, Subtrees),
    %%           ZNode = erl_syntax:revert(erl_syntax:update_tree(YNode, NSubTrees)),
    %%           F(post, ZNode)
    %%    ]).
    ast_monad:bind(
      Monad,
      F(PreType, XNode),
      fun(YNode) ->
              case erl_syntax:subtrees(YNode) of
                  [] ->
                      ast_monad:return(Monad, YNode);
                  Subtrees ->
                      ast_monad:bind(
                        Monad,
                        map_m(Monad, F, Subtrees),
                        fun(NSubTrees) ->
                                ZNode = erl_syntax:revert(erl_syntax:update_tree(YNode, NSubTrees)),
                                F(post, ZNode)
                        end)
              end
      end).

%% this method is from https://github.com/efcasado/forms/blob/master/src/forms.erl
-spec read(atom() | iolist()) -> [erl_parse:abstract_form()].
read(Module) when is_atom(Module) ->
    case beam_lib:chunks(code:which(Module), [abstract_code]) of
        {ok, {Module, [{abstract_code, {raw_abstract_v1, Forms}}]}} ->
            Forms;
        {ok, {no_debug_info, _}} ->
            throw({forms_not_found, Module});
        {error, beam_lib, {file_error, _, enoent}} ->
            throw({module_not_found, Module})
    end;
read(File) ->
    case epp:parse_file(File, []) of
        {ok, Forms} ->
            Forms;
        {ok, Forms, _Extra} ->
            Forms;
        {error, enoent} ->
            throw({file_not_found, File})
    end.

-spec attributes(atom(), _Forms) -> [_Attribute].

module_attributes(Attribute, Module) ->
    Attributes = Module:module_info(attributes),
    lists:foldl(
      fun({Attr, Value}, Acc) when Attr == Attribute ->
              [Value|Acc];
         (_Other, Acc) ->
              Acc
      end, [], Attributes).

attributes(Attribute, Forms) ->
    lists:foldl(
      fun({attribute, _Line, Attr, Values}, Acc) when Attr == Attribute ->
              [Values|Acc];
         (_Other, Acc) ->
              Acc
      end, [], Forms).

attributes_with_line(Attribute, Forms) ->
    lists:foldl(
      fun({attribute, Line, Attr, Values}, Acc) when Attr == Attribute ->
              [{Line, Values}|Acc];
         (_Other, Acc) ->
              Acc
      end, [], Forms).

from_value(Line, Tuple) when is_tuple(Tuple) ->
    {tuple, Line, lists:map(fun(Element) -> from_value(Line, Element) end, tuple_to_list(Tuple))};
from_value(Line, [H|T]) ->
    {cons, Line, from_value(Line, H), from_value(Line, T)};
from_value(Line, []) ->
    {nil, Line};
from_value(Line, '_') ->
    {var, Line, '_'};
from_value(Line, Value) when is_atom(Value) ->
    {atom, Line, Value};
from_value(Line, Value) when is_integer(Value) ->
    {integer, Line, Value};
from_value(Line, Value) when is_float(Value) ->
    {float, Line, Value}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
