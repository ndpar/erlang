% Copyright (c) 2009, Andrey Paramonov
% All rights reserved.
%
-module(id_generator).
-export([start/0, next/0, stop/0]).
-export([loop/1]).

start() ->
    register(id_generator, spawn(id_generator, loop, [0])), ok.

next() ->
    id_generator ! {next, self()},
    receive {reply, Result} -> Result end.

stop() ->
    id_generator ! {stop, self()},
    receive {reply, stopped} -> ok end.

loop(Count) ->
    receive
        {next, From} ->
            From ! {reply, Count},
            loop(Count + 1);
        {stop, From} ->
            From ! {reply, stopped}
    end.
