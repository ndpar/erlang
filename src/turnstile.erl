%%
%% http://www.objectmentor.com/resources/articles/umlfsm.pdf
%%
%% See also: turnstile2.erl
%%
%% 1> turnstile:start().
%% 2> turnstile:coin().
%% 3> turnstile:coin().
%% 4> turnstile:pass().
%% 5> turnstile:pass().
%%
-module(turnstile).
-export([start/0]).
-export([coin/0, pass/0]).
-export([init/0]).

start() -> register(turnstile, spawn(?MODULE, init, [])).

% Initial state

init() -> locked().

% Events

coin() -> turnstile ! coin.
pass() -> turnstile ! pass.

% States and transitions

locked() ->
    receive
        pass ->
            alarm(),
            locked();
        coin ->
            unlock(),
            unlocked()
    end.

unlocked() ->
    receive
        pass ->
            lock(),
            locked();
        coin ->
            thankyou(),
            unlocked()
    end.

% Actions

alarm() -> io:format("You shall not pass!~n").
unlock() -> io:format("Unlocking...~n").
lock() -> io:format("Locking...~n").
thankyou() -> io:format("Thank you for donation~n").
