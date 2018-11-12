%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created :  5 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(ast_macro).

%% API
-export([from_abstract/1]).

%%%===================================================================
%%% API
%%%===================================================================
from_abstract(Forms) when is_list(Forms) ->
    erl_prettypr:format(erl_syntax:form_list(Forms));
from_abstract(Form) ->
    erl_prettypr:format(erl_syntax:form_list([Form])).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
