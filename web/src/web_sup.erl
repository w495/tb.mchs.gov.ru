-module(web_sup).
-author('author <author@example.com>').

-behaviour(supervisor).

%% External exports
-export([start_link/0, upgrade/0]).

%% supervisor callbacks
-export([init/1]).

-include("../include/common.hrl").

%% @spec start_link() -> ServerRet
%% @doc API for starting the supervisor.
start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% @spec upgrade() -> ok
%% @doc Add processes if necessary.
upgrade() ->
    {ok, {_, Specs}} = init([]),

    Old = sets:from_list(
            [Name || {Name, _, _, _} <- supervisor:which_children(?MODULE)]),
    New = sets:from_list([Name || {Name, _, _, _, _, _} <- Specs]),
    Kill = sets:subtract(Old, New),

    sets:fold(fun (Id, ok) ->
                      supervisor:terminate_child(?MODULE, Id),
                      supervisor:delete_child(?MODULE, Id),
                      ok
              end, ok, Kill),

    [supervisor:start_child(?MODULE, Spec) || Spec <- Specs],
    ok.

%% @spec init([]) -> SupervisorTree
%% @doc supervisor callback.
init([]) ->
    AssistSrv = {
        assistSrv,
        {assistSrv, start_link, []},
        permanent, 5000, worker, [assistSrv]},

    CLog = {
        clog,
        {clog, start_link, []},
        permanent,
        1000,
        worker,
        [clog]
    },

    PgConPool = {
        pgConPoolMchs,
        {pgConPool, start_link, [
            config:get(mchs_db_user, "cff"),
            config:get(mchs_db_password, "cff"),
            config:get(mchs_db_name, "mchs"),
            config:get(mchs_db_host, "localhost")
        ]},
        permanent,
        1000,
        worker,
        [pgConPool]
    },
    Sab = {
        xslt,
        {xslt, start_link, [
%            "cbin/sablotron_adapter"
            "cbin/libxslt_adapter"
        ]},
        permanent,
        1000,
        worker,
        [xslt]
    },


    Processes = [
        CLog, % Web,
        AssistSrv,
        PgConPool,
        Sab
    ],
    {ok, {{one_for_one, 10, 10}, Processes}}.
