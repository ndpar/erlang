%%
%% F.Cesarini & S.Thomson, Erlang Programming, p.154.
%% Exercise 6-2: Reliable Mutex Semaphore
%%
%% See: http://groups.google.com/group/erlang-programming-book/browse_thread/thread/1d26b447f9db8f54
%%
%% 1> mutex_reliable:start().
%% true
%%
-module(mutex_reliable_test).
-export([test1/0, test2/0]).
-export([first/0, second/0]).

first() ->
    io:format("~p requesting mutex~n", [self()]),
    mutex_reliable:wait(),
    receive cont -> ok end,
    io:format("~p releasing mutex~n", [self()]),
    mutex_reliable:signal().

second() ->
    io:format("~p requesting mutex~n", [self()]),
    mutex_reliable:wait(),
    mutex_reliable:signal().

%% 2> mutex_reliable_test:test1().
%% <0.35.0> requesting mutex
%% <0.36.0> requesting mutex
%% linking to and locking for <0.35.0>
%% <0.35.0> releasing mutex
%% ok
%% signal from/unlinking <0.35.0>
%% linking to and locking for <0.36.0>
%% waiting process died <0.36.0>
%%
test1() ->
    F = spawn(mutex_reliable_test, first, []),
    S = spawn(mutex_reliable_test, second, []),
    receive after 1000 -> ok end,
    exit(S, kill),
    F ! cont,
    ok.

%% 3> mutex_reliable_test:test2().
%% <0.38.0> requesting mutex
%% <0.39.0> requesting mutex
%% linking to and locking for <0.38.0>
%% waiting process died <0.38.0>
%% ok
%% linking to and locking for <0.39.0>
%% signal from/unlinking <0.39.0>
%%
test2() ->
    F = spawn(mutex_reliable_test, first, []),
    spawn(mutex_reliable_test, second, []),
    receive after 1000 -> ok end,
    exit(F, kill),
    ok.
