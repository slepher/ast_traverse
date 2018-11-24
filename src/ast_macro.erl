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
-export([to_string/1]).
-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
to_string(Forms) when is_list(Forms) ->
    erl_prettypr:format(erl_syntax:form_list(Forms));
to_string(Form) ->
    erl_prettypr:format(erl_syntax:form_list([Form])).

parse_transform(Form, Opts) ->
    ast_macro_transfrom:parse_transform(Form, Opts).

format_error(Error) ->
    ast_macro_transfrom:format_error(Error).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
