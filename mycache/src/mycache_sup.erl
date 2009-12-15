-module(mycache_sup).
-export([start/0]).
-export([init/1]).
-behaviour(supervisor).

start() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    MycacheWorker = {mycache, {mycache, start, []}, permanent, 30000, worker, [mycache, mnesia]},
    {ok, {{one_for_all, 5, 3600}, [MycacheWorker]}}.
