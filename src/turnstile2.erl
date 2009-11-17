%%
%% Erlang Programming, p.125.
%% FSM Pattern.
%%
%% 1> turnstile2:start().
%% 2> turnstile2:coin().
%% 3> turnstile2:coin().
%% 4> turnstile2:pass().
%% 5> turnstile2:pass().
%% 6> turnstile2:stop().
%%
-module(turnstile2).
-export([start/0, stop/0]).
-export([coin/0, pass/0]).
-export([init/0]).

% Start/stop functions

start() -> register(turnstile2, spawn(?MODULE, init, [])).
stop() -> turnstile2 ! stop.

% Initial state

init() -> locked().

% Events

coin() -> turnstile2 ! coin.
pass() -> turnstile2 ! pass.

% States and transitions

locked() ->
    receive
        pass ->
            alarm(),
            locked();
        coin ->
            unlock(),
            unlocked();
        stop ->
            ok
    end.

unlocked() ->
    receive
        pass ->
            lock(),
            locked();
        coin ->
            thankyou(),
            unlocked();
        stop ->
            ok
    end.

% Actions

alarm() -> io:format("You shall not pass!~n").
unlock() -> io:format("Unlocking...~n").
lock() -> io:format("Locking...~n").
thankyou() -> io:format("Thank you for donation~n").
