%% Created: 16.01.2009
%% Description: Source from rabbitmq
-module(amnesia).

%%
%% Include files
%%
-include("../include/common.hrl").
-include("../include/db.hrl").

%%
%% Exported Functions
%%
-export([init/0, status/0, ensure_mnesia_dir/0, ensure_mnesia_running/0,
         cluster/1, reset/0, force_reset/0]).

%%
%% API Functions
%%
init() ->
    ok = ensure_mnesia_running(),
    ok = ensure_mnesia_dir(),
    ok = init_db(read_cluster_nodes_config()),
    ok = wait_for_tables(),
    ok.

status() ->
    DbNodes = mnesia:system_info(db_nodes),
    Running = mnesia:system_info(running_db_nodes),
    Stopped = DbNodes -- Running,
    [{nodes, DbNodes},
     {running_nodes, Running},
     {stopped_nodes, Stopped}].

%% Alter which disk nodes this node is clustered with. This can be a
%% subset of all the disk nodes in the cluster but can (and should)
%% include the node itself if it is to be a disk rather than a ram
%% node.
cluster(ClusterNodes) ->
  ok = ensure_mnesia_not_running(),
  ok = ensure_mnesia_dir(),
  ensure_ok(mnesia:start(), cannot_start_mnesia),
  try
    flog:info([{?MODULE,cluster},{nodes,ClusterNodes}]),
    ok = init_db(ClusterNodes),
    ok = wait_for_tables(),
    ok = create_cluster_nodes_config(ClusterNodes)
  after
    mnesia:stop()
  end,
  ok.

%% return node to its virgin state, where it is not member of any
%% cluster, has no cluster configuration, no local database, and no
%% persisted messages
reset()       -> reset(false).
force_reset() -> reset(true).

%%
%% Local Functions
%%

%% macros definded in ../include/db.hrl
global_tables()   -> ?GLOBAL_TABLES.
local_tables()    -> ?LOCAL_TABLES.

-ifdef(dynamic_tables).
dynamic_tables() ->
    TabList = lists:map(fun(Tab) -> {Tab,atom_to_list(Tab)} end,
                        mnesia:system_info(tables)),
    ClusterNodes = mnesia:system_info(db_nodes) -- [node()],
    FunPrefixes =
      fun(Prefix, AllAcc) ->
          F = fun({Tab,StrTab}, Acc) ->
                  case string:str(StrTab,Prefix) of
                1 ->
                    [dyn_tab_def(ClusterNodes, Tab)|Acc];
                _ -> Acc
              end
      end,
      [lists:foldl(F, [], TabList) | AllAcc]
    end,
    lists:flatten(lists:foldl(FunPrefixes, [],
                              ?DYNAMIC_TABLES_PREFIXES)).
-else.
dynamic_tables() -> [].
-endif.

-ifdef(dynamic_tables).
dyn_tab_def(ClusterNodes, Tab) ->
    Defs = mnesia:table_info(Tab,all),
    StorageType = get_storage_type_from_cluster(ClusterNodes, Tab),
    io:format("cluster (~p): table=~p storage_type=~p~n",
              [ClusterNodes, Tab, StorageType]),
    {Tab,  [{StorageType, [node()]},
            {type, proplists:get_value(type,Defs)},
            {record_name, proplists:get_value(record_name,Defs)},
            {attributes, proplists:get_value(attributes,Defs)}
           ]}.

get_storage_type_from_cluster([], _Tab) ->
    ram_copies;
get_storage_type_from_cluster([Node|T], Tab) ->
    case catch rpc:call(Node, mnesia, table_info, [Tab, storage_type]) of
  {'EXIT', _} -> get_storage_type_from_cluster(T, Tab);
  StorageType -> StorageType
    end.
-endif.

table_names(Tables) ->
    lists:map(fun ({Tab, _}) -> Tab end, Tables).

dir() -> mnesia:system_info(directory).

ensure_mnesia_dir() ->
    MnesiaDir = dir() ++ "/",
    case filelib:ensure_dir(MnesiaDir) of
        {error, Reason} ->
            throw({error, {cannot_create_mnesia_dir, MnesiaDir, Reason}});
        ok -> ok
    end.

ensure_mnesia_running() ->
    case mnesia:system_info(is_running) of
        yes -> ok;
        no -> throw({error, mnesia_not_running})
    end.

ensure_mnesia_not_running() ->
    case mnesia:system_info(is_running) of
        no -> ok;
        yes -> throw({error, mnesia_unexpectedly_running})
    end.

check_schema_integrity() ->
    AllTabsDef = global_tables()++
%                 local_tables()++
                 dynamic_tables(),
    check_table(AllTabsDef).

check_table([]) -> ok;
check_table([{Tab, TabArgs}|T]) ->
    case catch mnesia:table_info(Tab, version) of
  {'EXIT', {aborted,{no_exists,Tab,version}}} ->
      case mnesia:create_table(Tab, TabArgs) of
    {atomic, ok} -> check_table(T);
    {aborted, Reason} -> {error,Reason}
      end;
  {'EXIT', Reason} -> {error,Reason};
      _ -> check_table(T)
    end.


ensure_ok(ok, _) -> ok;
ensure_ok({error, Reason}, ErrorTag) -> throw({error, {ErrorTag, Reason}}).

%% The cluster node config file contains some or all of the disk nodes
%% that are members of the cluster this node is / should be a part of.
%%
%% If the file is absent, the list is empty, or only contains the
%% current node, then the current node is a standalone (disk)
%% node. Otherwise it is a node that is part of a cluster as either a
%% disk node, if it appears in the cluster node config, or ram node if
%% it doesn't.

cluster_nodes_config_filename() ->
    dir() ++ "/cluster_nodes.config".

create_cluster_nodes_config(ClusterNodes) ->
    FileName = cluster_nodes_config_filename(),
    Handle = case file:open(FileName, [write]) of
                 {ok, Device} -> Device;
                 {error, Reason} ->
                     throw({error, {cannot_create_cluster_nodes_config,
                                    FileName, Reason}})
             end,
    try
        ok = io:write(Handle, ClusterNodes),
        ok = io:put_chars(Handle, [$.])
    after
        case file:close(Handle) of
            ok -> ok;
            {error, Reason1} ->
                throw({error, {cannot_close_cluster_nodes_config,
                               FileName, Reason1}})
        end
    end,
    ok.

read_cluster_nodes_config() ->
    FileName = cluster_nodes_config_filename(),
    case file:consult(FileName) of
        {ok, [ClusterNodes]} -> ClusterNodes;
        {error, enoent} ->
            case application:get_env(cluster_config) of
                undefined -> [];
                {ok, DefaultFileName} ->
                    case file:consult(DefaultFileName) of
                        {ok, [ClusterNodes]} -> ClusterNodes;
                        {error, enoent} ->
                            flog:error(?FMT(
                              "default cluster config file ~p does not exist",
                              [DefaultFileName])),
                            [];
                        {error, Reason} ->
                            throw({error, {cannot_read_cluster_nodes_config,
                                           DefaultFileName, Reason}})
                    end
            end;
        {error, Reason} ->
            throw({error, {cannot_read_cluster_nodes_config,
                           FileName, Reason}})
    end.

delete_cluster_nodes_config() ->
    FileName = cluster_nodes_config_filename(),
    case file:delete(FileName) of
        ok -> ok;
        {error, enoent} -> ok;
        {error, Reason} ->
            throw({error, {cannot_delete_cluster_nodes_config,
                           FileName, Reason}})
    end.

%% Take a cluster node config and create the right kind of node - a
%% standalone disk node, or disk or ram node connected to the
%% specified cluster nodes.
init_db(ClusterNodes) ->
    case mnesia:change_config(extra_db_nodes, ClusterNodes -- [node()]) of
        {ok, []} ->
            case mnesia:system_info(use_dir) of
                true ->
                    case check_schema_integrity() of
                        ok -> ok;
                        {error, Reason} ->
                            %% we cannot use flog here since
                            %% it may not have been started yet
                            error_logger:warning_msg(
                              "schema integrity check failed: ~p~n"
                              "moving database to backup location "
                              "and recreating schema from scratch~n",
                              [Reason]),
                            ok = move_db(),
                            ok = create_schema()
                    end;
                false ->
                    ok = create_schema()
            end;
        {ok, [_|_]} ->
            IsDiskNode = ClusterNodes == [] orelse
                         lists:member(node(), ClusterNodes),
            ok = create_local_table_copy(schema, disc_copies),
            ok = wait_for_replicated_tables(),
            ok = create_local_tables(local_tables()),
            ok = create_local_table_copies(case IsDiskNode of
                                               true  -> disc;
                                               false -> ram
                                           end);
        {error, Reason} ->
            %% one reason we may end up here is if we try to join
            %% nodes together that are currently running standalone or
            %% are members of a different cluster
            throw({error, {unable_to_join_cluster,
                           ClusterNodes, Reason}})
    end.

%check_new_tables() ->
%    Check = fun({Tab, TabArgs}) ->
%                case catch(mnesia:table_info(Tab, type)) of
%              {'EXIT', _} ->
%                  case mnesia:create_table(Tab, TabArgs) of
%                {atomic, ok} -> ok;
%                {aborted, Reason} ->
%                    throw({error, {table_creation_failed,
%                                   Tab, TabArgs, Reason}});
%                Any ->
%                    throw({error, {table_creation_failed,
%                                   Tab, TabArgs, Any}})
%                  end;
%              _ -> ok
%                end
%    end,
%    lists:foreach(Check, local_tables()),  %% global & failsafe ???
%    ok.


create_schema() ->
    mnesia:stop(),
    ensure_ok(mnesia:create_schema([node()]), cannot_create_schema),
    ensure_ok(mnesia:start(), cannot_start_mnesia),
    create_tables().

move_db() ->
    mnesia:stop(),
    MnesiaDir = filename:dirname(dir() ++ "/"),
    {{Year, Month, Day}, {Hour, Minute, Second}} = erlang:universaltime(),
    BackupDir = lists:flatten(
                  io_lib:format("~s_~w~2..0w~2..0w~2..0w~2..0w~2..0w",
                                [MnesiaDir,
                                 Year, Month, Day, Hour, Minute, Second])),
    case file:rename(MnesiaDir, BackupDir) of
        ok ->
            %% we cannot use flog here since it may not have
            %% been started yet
            error_logger:warning_msg("moved database from ~s to ~s~n",
                                     [MnesiaDir, BackupDir]),
            ok;
        {error, Reason} -> throw({error, {cannot_backup_mnesia,
                                          MnesiaDir, BackupDir, Reason}})
    end,
    ok = ensure_mnesia_dir(),
    ensure_ok(mnesia:start(), cannot_start_mnesia),
    ok.

create_tables() ->
    lists:foreach(
        fun ({Tab, TabArgs}) ->
                case mnesia:create_table(Tab, TabArgs) of
                    {atomic, ok} -> ok;
                    {aborted, Reason} ->
                        throw({error, {table_creation_failed,
                                       Tab, TabArgs, Reason}});
                    Any ->
                        throw({error, {table_creation_failed,
                                       Tab, TabArgs, Any}})
                end
        end,
        local_tables() ++ global_tables() ),
    ok.

create_local_tables(Tables) ->
    lists:foreach(
        fun ({Tab, TabArgs}) ->
                case mnesia:create_table(Tab, TabArgs) of
                    {atomic, ok} -> ok;
                    {aborted, {already_exists, Tab}} -> ok;
                    {aborted, Reason} ->
                        throw({error, {table_creation_failed,
                                       Tab, TabArgs, Reason}});
                    Any ->
                        throw({error, {table_creation_failed,
                                       Tab, TabArgs, Any}})
                end
        end,
        Tables ),
    ok.

table_has_copy_type(TabDef, DiscType) ->
    lists:member(node(), proplists:get_value(DiscType, TabDef, [])).

create_local_table_copies(Type) ->
    lists:foreach(
      fun({Tab, TabDef}) ->
              HasDiscCopies     = table_has_copy_type(TabDef, disc_copies),
              HasDiscOnlyCopies = table_has_copy_type(TabDef, disc_only_copies),
              LocalTab          = proplists:get_bool(local_content, TabDef),
              StorageType =
                  if
                      Type =:= disc orelse LocalTab ->
                          if
                              HasDiscCopies     -> disc_copies;
                              HasDiscOnlyCopies -> disc_only_copies;
                              true              -> ram_copies
                          end;
%% unused code - commented out to keep dialyzer happy
%%                      Type =:= disc_only ->
%%                          if
%%                              HasDiscCopies or HasDiscOnlyCopies ->
%%                                  disc_only_copies;
%%                              true -> ram_copies
%%                          end;
                      Type =:= ram ->
                          ram_copies
                  end,
              ok = create_local_table_copy(Tab, StorageType)
      end,
      global_tables() ++ dynamic_tables() ),
    ok.

create_local_table_copy(Tab, Type) ->
    StorageType = mnesia:table_info(Tab, storage_type),
    {atomic, ok} =
        if
            StorageType == unknown ->
                mnesia:add_table_copy(Tab, node(), Type);
            StorageType /= Type ->
                mnesia:change_table_copy_type(Tab, node(), Type);
            true -> {atomic, ok}
        end,
    ok.

wait_for_replicated_tables() ->
    wait_for_tables(global_tables() ++ dynamic_tables() ).

wait_for_tables() ->
%    wait_for_tables(local_tables() ++
    wait_for_tables(global_tables() ++
                    dynamic_tables() ).

wait_for_tables(TabDefs) ->
    TableNames = table_names( TabDefs ),
    case check_schema_integrity() of
        ok ->
            case mnesia:wait_for_tables(TableNames, 200000) of
                ok -> ok;
                {timeout, BadTabs} ->
                    throw({error, {timeout_waiting_for_tables, BadTabs}});
                {error, Reason} ->
                    throw({error, {failed_waiting_for_tables, Reason}})
            end;
        {error, Reason} ->
            throw({error, {schema_integrity_check_failed, Reason}})
    end.

reset(Force) ->
    ok = ensure_mnesia_not_running(),
    Node = node(),
    case Force of
        true  -> ok;
        false ->
            ok = ensure_mnesia_dir(),
            ensure_ok(mnesia:start(), cannot_start_mnesia),
            {Nodes, RunningNodes} =
                try
                    ok = init(),
                    {mnesia:system_info(db_nodes) -- [Node],
                     mnesia:system_info(running_db_nodes) -- [Node]}
                after
                     mnesia:stop()
                end,
            leave_cluster(Nodes, RunningNodes),
            ensure_ok(mnesia:delete_schema([Node]),
                      cannot_delete_schema)
    end,
    ok = delete_cluster_nodes_config(),
    %% remove persistet messages and any other garbage we find
    lists:foreach(fun file:delete/1,
                  filelib:wildcard(mnesia:system_info(directory) ++ "/*")),
    ok.

leave_cluster([], _) -> ok;
leave_cluster(Nodes, RunningNodes) ->
    %% find at least one running cluster node and instruct it to
    %% remove our schema copy which will in turn result in our node
    %% being removed as a cluster node from the schema, with that
    %% change being propagated to all nodes
    case lists:any(
           fun (Node) ->
                   case rpc:call(Node, mnesia, del_table_copy,
                                 [schema, node()]) of
                       {atomic, ok} -> true;
                       {badrpc, nodedown} -> false;
                       {aborted, Reason} ->
                           throw({error, {failed_to_leave_cluster,
                                          Nodes, RunningNodes, Reason}})
                   end
           end,
           RunningNodes) of
        true -> ok;
        false -> throw({error, {no_running_cluster_nodes,
                                Nodes, RunningNodes}})
    end.

