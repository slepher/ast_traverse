%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 24 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(ast_expand_functions).

%% API
-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Options) ->
    File = filename:basename(ast_traverse:file(Forms)),
    FunctionMap = expand_functions(Forms, File),
    NForms = 
        lists:foldl(
          fun({function, Line, Name, 0, [{clause, _, [], [], Expressions}]} = Node, Acc) ->
                  case maps:find(Name, FunctionMap) of
                      {ok, Opts} ->
                          case walk_expand_function(Expressions, Opts, File, Line, Name) of
                              Nodes when is_list(Nodes) ->
                                  lists:reverse(Nodes) ++ Acc;
                              NNode ->
                                  [NNode|Acc]
                          end;
                      error ->
                          [Node|Acc]
                  end;
             (Node, Acc) ->
                  [Node|Acc]
          end, [], Forms),
    lists:reverse(NForms).

format_error(Message) ->
    case io_lib:deep_char_list(Message) of
        true -> Message;
        _    -> io_lib:write(Message)
    end.

%%--------------------------------------------------------------------
%% @doc
%% @spec
%% @end
%%--------------------------------------------------------------------

%%%===================================================================
%%% Internal functions
%%%===================================================================
walk_expand_function(Expressions, Opts, File, Line, Name) ->
    {value, Val, _} = erl_eval:exprs(Expressions, erl_eval:new_bindings()),
    case proplists:get_value(debug, Opts, false) of
        true ->
            io:format("from ~s:~p ~p~n", [File, Line, Name]),
            io:format("~s~n", [ast_macro:to_string(Val)]);
        false ->
            ok
    end,
    ast_quote:replace_line(Val, Line).

expand_functions(Forms, File) ->
    Functions = lists:flatten(ast_traverse:attributes_with_line(expand_functions, Forms)),
    lists:foldl(
      fun({_Line, {Function, Opts}}, Acc) when is_atom(Function), is_list(Opts) ->
              maps:put(Function, Opts, Acc);
         ({_Line, Function}, Acc) when is_atom(Function) ->
              maps:put(Function, [], Acc);
         ({Line, Other}, Acc) ->
              io:format("~s:~p invalid expand_functions attribute ~p~n",[File, Line, Other]),
              Acc
      end, maps:new(), Functions).
    
