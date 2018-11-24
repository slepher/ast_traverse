%%%-------------------------------------------------------------------
%%% @author Chen Slepher <slepheric@gmail.com>
%%% @copyright (C) 2018, Chen Slepher
%%% @doc
%%%
%%% @end
%%% Created : 18 Nov 2018 by Chen Slepher <slepheric@gmail.com>
%%%-------------------------------------------------------------------
-module(ast_macro_transfrom).

%% API
-export([parse_transform/2, format_error/1]).

%%%===================================================================
%%% API
%%%===================================================================
parse_transform(Forms, _Options) ->
    [{File, _}] = ast_traverse:attributes(file, Forms),
    Macros = macros(Forms),
    Replaces = replaces(Macros),
    NForms = 
        lists:foldl(
          fun({function, _Line, Name, 0, [{clause, _, [], [], [Expression]}]} = Node, Acc) ->
                  case maps:find(Name, Replaces) of
                      {ok, {Module, Function}} ->
                          case walk_node(Expression, maps:with([{Module, Function}], Macros), File) of
                              not_match ->
                                  [Node|Acc];
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
    ast_traverse:map(fun(Type, Node) -> walk_macros(Type, Node, Macros, File) end, lists:reverse(NForms)).

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
walk_macros(pre, Node, Macros, File) ->
    case walk_node(Node, Macros, File) of
        not_match ->
            Node;
        NNode ->
            NNode
    end;
walk_macros(_, Node, _Macros, _File) ->
    Node.

walk_node({call, Line, {remote, Line2, {atom, Line2, Module}, {atom, Line2, Function}}, Arguments}, Macros, File) ->
    case maps:find({Module, Function}, Macros) of
        {ok, Opts} ->
            Node = ast_quote:replace_line(Module:Function(Arguments), Line),
            format_node(File, Line, Module, Function, Node, Opts),
            Node;
        error -> 
            not_match
    end;
walk_node(_Node, _Macros, _File) ->
    not_match.

format_node(File, Line, Module, Function, Nodes, Opts) ->
    case proplists:get_value(debug, Opts, false) of
        true ->
            io:format("from ~s:~p ~p:~p~n", [filename:basename(File), Line, Module, Function]),
            io:format("~s~n", [ast_macro:to_string(Nodes)]);
        false ->
            ok
    end.

macros(Forms) ->
    Macros = lists:flatten(ast_traverse:attributes_with_line(import_macro, Forms)),
    lists:foldl(
      fun({Line, {Module, Function}}, Acc) ->
              add_macro(Module, Function, [], Line, Acc);
         ({Line, {Module, Function, Opts}}, Acc) when is_list(Opts)->
              add_macro(Module, Function, Opts, Line, Acc);
         ({Line, Other}, Acc) ->
              io:format("invalid import macro ~p at ~p~n", [Other, Line]),
              Acc
      end, maps:new(), Macros).

add_macro(Module, Function, Options, Line, Acc) ->
    case erlang:function_exported(Module, Function, 1) of
        true ->
            maps:put({Module, Function}, Options, Acc);
        false ->
            io:format("unexported macro ~p:~p at ~p~n", [Module, Function, Line]),
            Acc
    end.
              
replaces(Macros) ->
    maps:fold(
      fun({Module, Function}, Opts, Acc) ->
              case proplists:get_value(replace_functions, Opts, undefined) of
                  undefined ->
                      Acc;
                  ReplaceFunctions when is_list(ReplaceFunctions) ->
                      lists:foldl(
                        fun(Name, Acc1) ->
                                maps:put(Name, {Module, Function}, Acc1)
                        end, Acc, ReplaceFunctions);
                  FunctionName ->
                      maps:put(FunctionName, {Module, Function}, Acc)
              end
      end, maps:new(), Macros).
                      
