%%
%% F.Cesarini & S.Thomson, Erlang Programming, p.291.
%% Exercise 12-1: Database Server
%%
%% 1> mydb:start().
%% {ok,<0.33.0>}
%% 2> mydb:write(foo,bar).
%% ok
%% 3> mydb:read(baz).
%% {error, instance}
%% 4> mydb:read(foo).
%% {ok, bar}
%% 5> mydb:match(bar).
%% [foo]
%% 7> mydb:stop().
%% ok
%%
-module(mydb).
-export([start/0, stop/0]).
-export([write/2, delete/1, read/1, match/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2]).
-behaviour(gen_server).

% Start/stop functions

start() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

stop() ->
    gen_server:cast(?MODULE, stop).

% Functional interface

write(Key, Element) ->
    gen_server:call(?MODULE, {create, Key, Element}).

delete(Key) ->
    gen_server:call(?MODULE, {delete, Key}).

read(Key) ->
    gen_server:call(?MODULE, {read, Key}).

match(Element) ->
    gen_server:call(?MODULE, {find_by_element, Element}).

% Callback functions

init(_) ->
    {ok, db:new()}.

terminate(_Reason, Db) ->
    db:destroy(Db).

handle_cast(stop, Db) ->
    {stop, normal, Db}.

handle_call({create, Key, Element}, _From, Db) ->
    {reply, ok, db:write(Key, Element, Db)};

handle_call({delete, Key}, _From, Db) ->
    {reply, ok, db:delete(Key, Db)};

handle_call({read, Key}, _From, Db) ->
    {reply, db:read(Key, Db), Db};

handle_call({find_by_element, Element}, _From, Db) ->
    {reply, db:match(Element, Db), Db}.
