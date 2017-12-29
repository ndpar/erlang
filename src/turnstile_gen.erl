%%
%% See also: turnstile.erl
%%
-module(turnstile_gen).
-export([start/0, stop/0]).
-export([coin/0, pass/0]).
-export([init/1, callback_mode/0, locked/3, unlocked/3]).
-behaviour(gen_statem).

% Start/stop functions

start() ->
    gen_statem:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_statem:stop(?MODULE).

% Functional interface

coin() ->
    gen_statem:call(?MODULE, coin).

pass() ->
    gen_statem:call(?MODULE, pass).

% Callback functions

init(_) ->
    {ok, locked, []}.

callback_mode() ->
    state_functions.

locked({call, From}, coin, _) ->
    do_unlock(),
    {next_state, unlocked, [], {reply, From, unlocked}};

locked({call, From}, pass, _) ->
    do_alarm(),
    {keep_state, [], {reply, From, locked}}.

unlocked({call, From}, coin, _) ->
    do_thankyou(),
    {keep_state, [], {reply, From, unlocked}};

unlocked({call, From}, pass, _) ->
    do_lock(),
    {next_state, locked, [], {reply, From, locked}}.

% Actions

do_alarm() -> io:format("You shall not pass!~n").
do_unlock() -> io:format("Unlocking...~n").
do_lock() -> io:format("Locking...~n").
do_thankyou() -> io:format("Thank you for your donation~n").
