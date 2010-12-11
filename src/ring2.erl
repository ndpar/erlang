-module(ring2).
-export([start/3]).
-export([loop/1, loop/2]).


start(N, M, Message) when N > 1 -> spawn(fun() -> ring(N-1, M, Message) end).

ring(N, M, Message) when M > 0 ->
    P = lists:foldl(fun(_, Proc) -> spawn(fun() -> loop(Proc) end) end, self(), lists:seq(1, N)),
    trace(P, Message),
    P ! {message, Message},
    loop(P, M).

loop(P) ->
    receive
        {message, Message} ->
            trace(P, Message),
            P ! {message, Message},
            loop(P);
        stop ->
            P ! stop
    end.

loop(P, 1) -> P ! stop;
loop(P, M) ->
    receive
        {message, Message} ->
            trace(P, Message),
            P ! {message, Message},
            loop(P, M-1)
    end.

trace(P, Message) -> io:format("~p -> ~p: ~p~n", [self(), P, Message]).

