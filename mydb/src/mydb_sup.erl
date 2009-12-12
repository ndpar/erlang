%%
%% F.Cesarini & S.Thomson, Erlang Programming, p.292.
%% Exercise 12-2: Database Supervisor
%%
%% 1> mydb_sup:start().
%% {ok,<0.33.0>}
%% 2> mydb:write(foo,bar).
%% ok
%% 3> mydb:match(bar).
%% [foo]
%%
-module(mydb_sup).
-export([start/0]).
-export([init/1]).
-behaviour(supervisor).

start() ->
    supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init(_) ->
    MydbWorker = {mydb, {mydb, start, []}, permanent, 30000, worker, [mydb, db]},
    {ok, {{one_for_all, 5, 3600}, [MydbWorker]}}.

%% Killing supervisor kills worker
%%
%% 1> mydb_sup:start().
%% {ok,<0.48.0>}
%% 2> whereis(mydb).
%% <0.49.0>
%% 3> exit(whereis(mydb_sup), kill).
%% ** exception exit: killed
%% 4> whereis(mydb).
%% undefined

%% Supervisor restarts killed worker
%%
%% 1> mydb_sup:start().
%% {ok,<0.70.0>}
%% 2> whereis(mydb).
%% <0.71.0>
%% 3> exit(whereis(mydb), kill).
%% true
%% 4> whereis(mydb).
%% <0.74.0>

%% Supervisor cannot survive 6 worker deaths
%%
%% 1> mydb_sup:start().
%% {ok,<0.33.0>}
%% 2> exit(whereis(mydb), kill).
%% true
%% 3> exit(whereis(mydb), kill).
%% true
%% 4> exit(whereis(mydb), kill).
%% true
%% 5> exit(whereis(mydb), kill).
%% true
%% 6> exit(whereis(mydb), kill).
%% true
%% 7> exit(whereis(mydb), kill).
%% ** exception exit: shutdown
%% 8> whereis(mydb_sup).
%% undefined
