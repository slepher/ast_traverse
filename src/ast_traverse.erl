%% ``Licensed under the Apache License, Version 2.0 (the "License");
%% you may not use this file except in compliance with the License.
%% You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%% 
%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 30 Aug 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(ast_traverse).
-export([traverse/3, map_reduce/3]).
-export([map_with_state/3, map/2, reduce/3]).
-export([read/1, attributes/2]).

-spec traverse(M, fun((_Type, _NodeType, Node) -> monad:monadic(M, Node)), Form) ->
                      monad:monadic(M, Form) when M :: monad:monad().
traverse(Monad, F, Forms) ->
    do_traverse(Monad, F, Forms, forms).

-spec map_reduce(fun((_Type, _NodeType, Node, State) -> {Node, State}), State, Form) -> {Form, State}.
map_reduce(F, Init, Forms) ->
    STNode = traverse(ast_state, fun(Type, NodeType, Node) -> fun(State) -> F(Type, NodeType, Node, State) end end, Forms),
    ast_state:run(STNode, Init).

-spec map_with_state(fun((_Type, _NodeType, Node, State) -> {Node, State}), State, Forms) -> Forms.
map_with_state(F, Init, Forms) ->
    {NForms, _State} = map_reduce(F, Init, Forms),
    NForms.

-spec map(fun((_Type, _NodeType, Node) -> Node), Forms) -> Forms.
map(F, Forms) ->
    map_with_state(fun(Type, NodeType, Node, State) -> {F(Type, NodeType, Node), State} end, ok, Forms).

-spec reduce(fun((_Type, _NodeType, _Node, State) -> State), State, _Forms) -> State.
reduce(F, Init, Forms) ->
    {_NForms, NState} = map_reduce(fun(Type, NodeType, Node, State) -> {Node, F(Type, NodeType, Node, State)} end, Init, Forms),
    NState.

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

attributes(Attribute, Forms) ->
    lists:foldl(
      fun({attribute, _Line, Attr, Values}, Acc) when Attr == Attribute ->
              [Values|Acc];
         (_Other, Acc) ->
              Acc
      end, [], Forms).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_traverse(Monad, F, XNode, NodeType) ->
    %% do form
    %% do([Monad ||
    %%           YNode <- F(pre, XNode),
    %%           ZNode <- fold_children(Monad, F, YNode, ast_lens:node_lens(XNodeType, YNode)),
    %%           F(post, ZNode)
    %%    ]).
    ast_monad:bind(
      Monad,
      F(pre, NodeType, XNode),
      fun(YNode) ->
              ast_monad:bind(
                Monad,
                %% type of y node should be same as type of x node
                fold_children(Monad, F, YNode, NodeType),
                fun(ZNode) ->
                        F(post, NodeType, ZNode)
                end)
      end).

fold_children(Monad, F, Node, NodeType) ->
    ChildrenLens = ast_lens:children_lens(NodeType, Node),
    lists:foldl( 
      fun({ChildNodeType, ChildLens}, MNode) ->
              ast_monad:bind(
                Monad, MNode,
                fun(NodeAcc) ->
                        (ast_lens:modify(Monad, ChildLens, 
                                         fun(Child) ->
                                                 do_traverse(Monad, F, Child, ChildNodeType)
                                         end))(NodeAcc)
                end)
      end, ast_monad:return(Monad, Node), ChildrenLens).
