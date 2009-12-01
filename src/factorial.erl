%
% 1> factorial:start().
% 2> factorial:next().
% 3> factorial:next().
% ...
%
-module(factorial).
-export([start/0, next/0]).
-export([init/0]).

start() ->
    register(factorial, spawn(?MODULE, init, [])).

next() ->
    factorial ! {next, self()},
    receive N -> N end.

init() ->
    loop(next(1, 1)).

next(Acc, N) ->
    fun() -> [Acc * N | next(Acc * N, N + 1)] end.

loop(Fun) ->
    receive
        {next, From} ->
            [N | NewFun] = Fun(),
            From ! N,
            loop(NewFun)
    end.
