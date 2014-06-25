-module(metric_frontend_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type), {I, {I, start_link, []}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([]) ->
    {Host, Port} =
        case application:get_env(metric_frontend, backend) of
            {ok, R} ->
                R;
            _ ->
                {"127.0.0.1", 5555}
        end,
    Name = backend_connection,
    SizeArgs = [
                {size, 10},
                {max_overflow, 20}
               ],
    PoolArgs = [{name, {local, Name}},
                {worker_module, metric_connection}] ++ SizeArgs,
    WorkerArgs = [Host, Port],
    %%C = {metric_connection, {metric_connection, start_link, [Host, Port]},
    %%     permanent, 5000, worker, [metric_connection]},
    {ok, {{one_for_one, 5, 10}, [poolboy:child_spec(Name, PoolArgs, WorkerArgs)]}}.
