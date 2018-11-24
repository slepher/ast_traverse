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
    File = file(Forms),
    Macros = macros(Forms, File),
    ast_traverse:map(fun(Type, Node) -> walk_macros(Type, Node, Macros, File) end, Forms).

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

macros(Forms, File) ->
    Macros = lists:flatten(ast_traverse:attributes_with_line(import_macro, Forms)),
    lists:foldl(
      fun({Line, {Module, Function}}, Acc) ->
              add_macro(Module, Function, [], File, Line, Acc);
         ({Line, {Module, Function, Opts}}, Acc) when is_list(Opts)->
              add_macro(Module, Function, Opts, File, Line, Acc);
         ({Line, Other}, Acc) ->
              io:format("invalid import macro ~p at ~p~n", [Other, Line]),
              Acc
      end, maps:new(), Macros).

add_macro(Module, Function, Options, File, Line, Acc) ->
    case get_exports(Module) of
        {ok, Exports} ->
            case lists:member({Function, 1}, Exports) of
                true ->
                    maps:put({Module, Function}, Options, Acc);
                false ->
                    io:format("~s:~p unexported macro ~p:~p~n", [File, Line, Module, Function]),
                    Acc
            end;
        {error, undef} ->
            Msg = "~s:~p module ~p could not be loaded, add to erl_first_files in rebar.config to make it compile first.~n",
            io:format(Msg, [File, Line, Module]),
            Acc
    end.

get_exports(Module) ->
    try
        {ok, Module:module_info(exports)}
    catch
        _:undef ->
            {error, undef}
    end.
                      
file(Forms) ->
    [{File, _}] = ast_traverse:attributes(file, Forms),
    filename:basename(File).
