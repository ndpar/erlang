%
% F.Cesarini & S.Thomson, Erlang Programming, p.169.
% Exercise 7-3: Database of Records
%
-module(db3_server).
-export([start/0, stop/0]).
-export([write/1, delete/1, read/1, match/1]).
-export([init/0]).

% Start/stop functions

start() ->
    register(db3_server, spawn(?MODULE, init, [])), ok.

stop() ->
    db3_server ! stop, ok.

init() ->
    loop(db3:new()).

% Functional interface

write(Data) ->
    call(create, Data).

delete(Data) ->
    call(delete, Data).

read(Data) ->
    call(read, Data).

match(Data) ->
    call(find_by_element, Data).

call(Command, Parameter) ->
    db3_server ! {request, self(), Command, Parameter},
    receive {reply, Reply} -> Reply end.

% Main loop

loop(Db) ->
    receive
        stop ->
            db3:destroy(Db);
        {request, Pid, create, Data} ->
            NewDb = db3:write(Db, Data),
            reply(Pid, ok),
            loop(NewDb);
        {request, Pid, delete, Data} ->
            NewDb = db3:delete(Db, Data),
            reply(Pid, ok),
            loop(NewDb);
        {request, Pid, read, Data} ->
            Entry = db3:read(Db, Data),
            reply(Pid, Entry),
            loop(Db);
        {request, Pid, find_by_element, Data} ->
            Entries = db3:match(Db, Data),
            reply(Pid, Entries),
            loop(Db)
    end.

reply(Pid, Msg) -> Pid ! {reply, Msg}.
