%%
%% F.Cesarini & S.Thomson, Erlang Programming, p.137.
%% Exercise 5-1: Database Server
%%
%% 1> my_db:start().
%% ok
%% 2> my_db:write(foo,bar).
%% ok
%% 3> my_db:read(baz).
%% {error, instance}
%% 4> my_db:read(foo).
%% {ok, bar}
%% 5> my_db:match(bar).
%% [foo]
%%
-module(my_db).
-export([start/0, stop/0]).
-export([write/2, delete/1, read/1, match/1]).
-export([init/0]).

% Start/stop functions

start() ->
    register(my_db, spawn(?MODULE, init, [])), ok.

stop() ->
    my_db ! stop, ok.

init() ->
    loop(db:new()).

% Functional interface

write(Key, Element) ->
    call(create, {Key, Element}).

delete(Key) ->
    call(delete, Key).

read(Key) ->
    call(read, Key).

match(Element) ->
    call(find_by_element, Element).

call(Command, Parameter) ->
    my_db ! {request, self(), Command, Parameter},
    receive {reply, Reply} -> Reply end.

% Main loop

loop(Db) ->
    receive
        stop ->
            db:destroy(Db);
        {request, Pid, create, {Key, Element}} ->
            NewDb = db:write(Key, Element, Db),
            reply(Pid, ok),
            loop(NewDb);
        {request, Pid, delete, Key} ->
            NewDb = db:delete(Key, Db),
            reply(Pid, ok),
            loop(NewDb);
        {request, Pid, read, Key} ->
            Entry = db:read(Key, Db),
            reply(Pid, Entry),
            loop(Db);
        {request, Pid, find_by_element, Element} ->
            Entries = db:match(Element, Db),
            reply(Pid, Entries),
            loop(Db)
    end.

reply(Pid, Msg) -> Pid ! {reply, Msg}.
