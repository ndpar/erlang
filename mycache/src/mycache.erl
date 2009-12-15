-module(mycache).
-export([start/0, stop/0]).
-export([put/2, get/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-behaviour(gen_server).
-include("mycache.hrl").

% Start/stop functions

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

% Functional interface

put(Key, Value) ->
    gen_server:call(?MODULE, {put, Key, Value}).

get(Key) ->
    gen_server:call(?MODULE, {get, Key}).

% Callback functions

init(_) ->
    application:start(mnesia),
    mnesia:wait_for_tables([mycache], infinity),
    {ok, []}.

terminate(_Reason, _State) ->
    application:stop(mnesia).

handle_cast(stop, State) ->
    {stop, normal, State}.

handle_call({put, Key, Value}, _From, State) ->
    Rec = #mycache{key = Key, value = Value},
    {_, Result} = mnesia:transaction(fun() -> mnesia:write(Rec) end),
    {reply, Result, State};

handle_call({get, Key}, _From, State) ->
    case mnesia:dirty_read({mycache, Key}) of
        [#mycache{value = Value}] -> {reply, Value, []};
        _ -> {reply, null, State}
    end.
