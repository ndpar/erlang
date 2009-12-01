%%
%% F.Cesarini & S.Thomson, Erlang Programming, p.129.
%% FSM Pattern
%%
%% 1> mutex:start().
%% 2> mutex:wait().
%% 3> mutex:signal().
%% 4> mutex:stop().
%%
-module(mutex).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

start() -> register(mutex, spawn(?MODULE, init, [])).

% Initial state

init() -> free().

% Events

wait() ->
    mutex ! {wait, self()},
    receive ok -> ok end.

signal() -> mutex ! {signal, self()}, ok.

stop() -> mutex ! stop.

% States and transitions

free() ->
    receive
        {wait, Pid} ->
            Pid ! ok,
            busy(Pid);
        stop ->
            terminate()
    end.

busy(Pid) ->
    receive
        {signal, Pid} -> free()
    end.

terminate() ->
    receive
        {wait, Pid} ->
            exit(Pid, kill),
            terminate()
    after
        0 -> ok
    end.