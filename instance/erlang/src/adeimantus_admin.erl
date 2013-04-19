%% adeimantus admin helper functions.
%%
-module(adeimantus_admin).

-export([start/0, add_node/1, remove_node/0, remove_node/1, hot_reload/1, cleanup_nodes/0]).

%% @doc start to initialize values, needed when running erlang from the command line.
-spec start() ->
                   ok.
start() ->
    ok.

%% @doc Add this node to a running cluster where the given node is running.
-spec add_node(atom() | [atom()]) ->
                      ok | {error, string()}.
add_node([RNode]) ->
    add_node(RNode);
add_node(RNode) ->
    error_logger:info_msg("Add ~p to cluster where ~p lives.~n",
                          [node(), RNode]),
    case net_adm:ping(RNode) of
        pong ->
            %% Set master node
            mnesia:set_master_nodes([RNode]),
            mnesia:start(),
            %% Add RNode to cluser
            {ok, _} = mnesia:change_config(extra_db_nodes, [RNode]),
            %% schema is in ram so we need to change to disk
            {atomic, ok} = mnesia:change_table_copy_type(
                             schema, node(), disc_copies),
            %% Copy tables from remote node
            lists:foreach(fun(Tab) when Tab =/= schema ->
                                  case mnesia:table_info(
                                         Tab,
                                         where_to_commit) of
                                      [] ->
                                          ok;
                                      [{_, Type} | _] ->
                                          mnesia:add_table_copy(Tab,
                                                                node(),
                                                                Type)
                                  end;
                             (schema) -> ok
                          end,
                          mnesia:system_info(tables)),
            ok;
        pang ->
            {error, nodedown}
    end.

%% @doc Remove the given node from the cluster.
%% It needs to be down or mnesia stopped on the node.
-spec remove_node() ->
                         ok.
remove_node() ->
    remove_node(node()).

-spec remove_node(atom() | [atom()]) ->
                         ok.
remove_node([RNode]) ->
    remove_node(RNode);
remove_node(RNode) ->
    error_logger:info_msg("Remove ~p from cluster. ~n",
                          [RNode]),
    case net_adm:ping(RNode) of
        pang ->
            mnesia:del_table_copy(schema, RNode);
        Err -> {error, Err}
    end.

cleanup_nodes() ->
    StoppedNodes = mnesia:system_info(db_nodes)--mnesia:system_info(running_db_nodes),
    lists:foreach(fun remove_node/1, StoppedNodes).


hot_reload([RNode|_]) ->
    hot_reload(RNode);

hot_reload(RNode) ->
    AllMods = rpc:call(RNode, code, all_loaded, []),
    EjabberdMods =
    lists:filter(fun is_adeimantus_mod/1, AllMods),
    ModList = [M || {M,_P} <- EjabberdMods],
    io:format("Reloading modules on node ~p: ~p~n", [ RNode, ModList ]),
    try
        lists:map(fun(M) ->
                    rpc:call(RNode, code, purge, [M])
                end, ModList),
        lists:map(fun(M) ->
                    rpc:call(RNode, code, load_file, [M])
                end, ModList)
    catch
        _:Err ->
            io:format("Caught error reloading code!: ~p~n~p~n",[Err,erlang:get_stacktrace()]),
            error
    end.


is_ejabberd_mod({_M,P}) when not is_list(P) ->
    false;
is_ejabberd_mod({_M,P}) ->
    case re:run(P, "ejabberd") of
        nomatch -> false;
        _ -> true
    end.

is_adeimantus_mod({adeimantus_admin, _P}) -> true;
is_adeimantus_mod({mod_push, _P}) -> true;
is_adeimantus_mod(_) -> false.

