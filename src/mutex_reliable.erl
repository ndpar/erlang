%%
%% F.Cesarini & S.Thomson, Erlang Programming, p.154.
%% Exercise 6-2: Reliable Mutex Semaphore
%%
%% See: http://groups.google.com/group/erlang-programming-book/browse_thread/thread/1d26b447f9db8f54
%%
-module(mutex_reliable).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

start() ->
    register(mutex_reliable, spawn(?MODULE, init, [])).

stop() ->
    mutex_reliable ! stop.

init() ->
    process_flag(trap_exit, true),
    free().

% Events

wait() ->
    mutex_reliable ! {wait, self()},
    receive ok -> ok end.

signal() ->
    mutex_reliable ! {signal, self()}, ok.

% States and transitions

free() ->
    receive
        {wait, Pid} ->
            io:format("linking to and locking for ~p~n", [Pid]),
            link(Pid),
            Pid ! ok,
            busy(Pid);
        {'EXIT', _Pid, normal} ->
            free();
        stop ->
            terminate()
    end.

busy(Pid) ->
    receive
        {signal, Pid} ->
            io:format("signal from/unlinking ~p~n", [Pid]),
            unlink(Pid),
            free();
        {'EXIT', Pid, _Reason} ->
            io:format("waiting process died ~p~n", [Pid]),
            free()
    end.

terminate() ->
    receive
        {wait, Pid} ->
            exit(Pid, kill),
            terminate()
    after
        0 -> ok
    end.
