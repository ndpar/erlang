%%
%% See also: turnstile.erl
%%
-module(turnstile_gen).
-export([start/0]).
-export([coin/0, pass/0]).
-export([init/1, locked/3, unlocked/3]).
-behaviour(gen_fsm).

% Start/stop functions

start() ->
    gen_fsm:start_link({local, ?MODULE}, ?MODULE, [], []).

% Functional interface

coin() ->
    gen_fsm:sync_send_event(?MODULE, coin).

pass() ->
    gen_fsm:sync_send_event(?MODULE, pass).

% Callback functions

init(_) ->
    {ok, locked, []}.

locked(coin, _, _) ->
    {reply, unlock(), unlocked, []};

locked(pass, _, _) ->
    {reply, alarm(), locked, []}.

unlocked(coin, _, _) ->
    {reply, thankyou(), unlocked, []};

unlocked(pass, _, _) ->
    {reply, lock(), locked, []}.

% Actions

alarm() -> io:format("You shall not pass!~n").
unlock() -> io:format("Unlocking...~n").
lock() -> io:format("Locking...~n").
thankyou() -> io:format("Thank you for donation~n").
