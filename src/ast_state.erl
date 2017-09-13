%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2017, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 13 Sep 2017 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(ast_state).
-export(['>>='/2, return/1]).
-export([run/2]).

'>>='(X, Fun) -> 
    fun (S) -> 
            {A, S1} = run(X, S),
            run(Fun(A), S1)
    end.

return(A) -> 
    fun (S) ->
            {A, S}
    end.

run(Monad, S)  -> 
    Monad(S).
