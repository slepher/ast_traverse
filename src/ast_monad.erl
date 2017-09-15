%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 13 Sep 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(ast_monad).

-export_type([monad/0, monadic/2]).

%% API
-export([lift_m/3, bind/3, return/2]).

-type monad()         :: module() | {module(), monad()}.
-type monadic(_M, _A) :: any().

%%%===================================================================
%%% API
%%%===================================================================
lift_m(Monad, F, X) ->
    bind(Monad, X,
         fun(A) ->
                 return(Monad, F(A))
         end).

%% same as monad:bind/3
bind({T, _IM} = M, X, F) ->
    T:'>>='(X, F, M);
bind(M, X, F) ->
    M:'>>='(X, F).

return({T, _IM} = M, A) ->
    T:return(A, M);
return(M, A) ->
    M:return(A).
%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
