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
%% The Initial Developer of the Original Code is Ericsson Utvecklings AB.
%% Portions created by Ericsson are Copyright 1999, Ericsson Utvecklings
%% AB. All Rights Reserved.''
%% 
%%     $Id$
%%
-module(erlando_ast).

%% An identity transformer of Erlang abstract syntax.

%% This module only traverses legal Erlang code. This is most noticeable
%% in guards where only a limited number of expressions are allowed.
%% N.B. if this module is to be used as a basis for transforms then
%% all the error cases must be handled otherwise this module just crashes!

-export([read/1, attributes/2, map_reduce/3, traverse/2, traverse/3]).

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

-spec map_reduce(fun((_Type, Node, State) -> error_m:error_m(any(), {Node, State})), State, Form) ->
                        error_m:error_m(any(), {Form, State}).
map_reduce(F, Init, Forms) ->
    ST = state_t:new(identity_m),
    STNode = traverse(
               ST, fun(Type, Node) -> state_t:state_t(fun(State) -> F(Type, Node, State) end) end, Forms),
    state_t:run_state(STNode, Init, ST).

-spec traverse(fun((Node, _Type) -> Node), Form) -> Form.
traverse(F, Forms) ->
    traverse(identity_m, F, Forms).

-spec traverse(M, fun((_Type, Node) -> monad:monadic(M, Node)), Form) -> monad:monadic(M, Form) when M :: monad:monad().
traverse(Monad, F, Forms) ->
    do_traverse(Monad, F, Forms, fun erlando_ast_lens:forms/1).

%%%===================================================================
%%% Internal functions
%%%===================================================================
do_traverse(Monad, F, XNode, Visitor) ->
    %% do form
    %% do([Monad ||
    %%           YNode <- F(pre, XNode),
    %%           ZNode <- fold_children(Monad, F, YNode, Visitor),
    %%           F(post, ZNode)
    %%    ]).
    monad:bind(
      Monad,F(pre, XNode),
      fun(YNode) ->
              monad:bind(
                Monad,
                fold_children(Monad, F, YNode, Visitor(YNode)),
                fun(ZNode) ->
                        F(post, ZNode)
                end)
      end).

fold_children(Monad, F, Node, ChildrenLens) ->
    lists:foldl( 
      fun({Visitor, ChildLens}, MNode) ->
              monad:bind(
                Monad, MNode,
                fun(NodeAcc) ->
                        (erlando_ast_lens:modify(Monad, ChildLens, 
                                         fun(Child) ->
                                                 do_traverse(Monad, F, Child, Visitor)
                                         end))(NodeAcc)
                          end)
      end, monad:return(Monad, Node), ChildrenLens).
