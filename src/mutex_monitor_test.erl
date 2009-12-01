%%
%% F.Cesarini & S.Thomson, Erlang Programming, p.154.
%% Exercise 6-2: Reliable Mutex Semaphore
%%
%% See also: mutex_reliable_test.erl
%%
%% 1> mutex_monitor:start().
%% true
%%
-module(mutex_monitor_test).
-export([test1/0, test2/0]).
-export([first/0, second/0]).

first() ->
    io:format("~p requesting mutex~n", [self()]),
    mutex_monitor:wait(),
    receive cont -> ok end,
    io:format("~p releasing mutex~n", [self()]),
    mutex_monitor:signal().

second() ->
    io:format("~p requesting mutex~n", [self()]),
    mutex_monitor:wait(),
    mutex_monitor:signal().

%% 2> mutex_monitor_test:test1().
%% <0.35.0> requesting mutex
%% <0.36.0> requesting mutex
%% Monitoring and locking for <0.35.0>
%% <0.35.0> releasing mutex
%% ok
%% Signal from and demonitor <0.35.0>
%% Monitoring and locking for <0.36.0>
%% Waiting process died <0.36.0>: noproc
%%
test1() ->
    F = spawn(mutex_monitor_test, first, []),
    S = spawn(mutex_monitor_test, second, []),
    receive after 1000 -> ok end,
    exit(S, kill),
    F ! cont,
    ok.

%% 3> mutex_monitor_test:test2().
%% <0.38.0> requesting mutex
%% <0.39.0> requesting mutex
%% Monitoring and locking for <0.38.0>
%% Waiting process died <0.38.0>: killed
%% ok
%% Monitoring and locking for <0.39.0>
%% Signal from and demonitor <0.39.0>
%%
test2() ->
    F = spawn(mutex_monitor_test, first, []),
    spawn(mutex_monitor_test, second, []),
    receive after 1000 -> ok end,
    exit(F, kill),
    ok.
