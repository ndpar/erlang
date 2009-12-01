%%
%% F.Cesarini & S.Thomson, Erlang Programming, p.154.
%% Exercise 6-2: Reliable Mutex Semaphore
%%
%% See also: mutex_reliable.erl
%%
-module(mutex_monitor).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

start() ->
    register(mutex_monitor, spawn(?MODULE, init, [])).

stop() ->
    mutex_monitor ! stop.

init() ->
%    process_flag(trap_exit, true),
    free().

% Events

wait() ->
    mutex_monitor ! {wait, self()},
    receive ok -> ok end.

signal() ->
    mutex_monitor ! {signal, self()}, ok.

% States and transitions

free() ->
    receive
        {wait, Pid} ->
            io:format("Monitoring and locking for ~p~n", [Pid]),
            Reference = erlang:monitor(process, Pid),
            Pid ! ok,
            busy(Pid, Reference);
%        {'EXIT', _Pid, normal} ->
%            free();
        stop ->
            terminate()
    end.

busy(Pid, Reference) ->
    receive
        {signal, Pid} ->
            io:format("Signal from and demonitor ~p~n", [Pid]),
            erlang:demonitor(Reference, [flush]),
            free();
        {'DOWN', Reference, process, Pid, Reason} ->
            io:format("Waiting process died ~p: ~s~n", [Pid, Reason]),
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
