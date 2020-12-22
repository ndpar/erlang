%
% [A2] Exercise 12.3, p. 198.
%
% Compile with trace disabled:
% 1> c(ring2, {d, no_trace}).
% 2> ring2:measure_time(10000, 10000, hello).
%
-module(ring2).
-export([measure_time/3, start/3]).

-ifndef(no_trace).
-define(TRACE(P, M, Message), io:format("~p -> ~p: ~p (~p)~n", [self(), P, Message, M])).
-else.
-define(TRACE(P, M, Message), void).
-endif.

measure_time(N, M, Message) ->
    {Micros, ok} = timer:tc(?MODULE, start, [N, M, Message]),
    io:format("Done in ~p µs. Message passing speed ~p s~n", [Micros, Micros / N / M / 1000000.0]).

%
% Creates N processes in a ring.
% Sends a message round the ring M times
% so that a total of N*M messages get sent.
%
start(N, M, Message) when N > 1 ->
    {_Pid, MonitorRef} = spawn_monitor(fun() -> ring(N-1, M, Message) end),
    receive
        {_Tag, MonitorRef, _Type, _Object, _Info} -> ok
    end.

ring(N, M, Message) when M > 0 ->
    P = lists:foldl(fun(_, Proc) -> spawn(fun() -> loop(Proc) end) end, self(), lists:seq(1, N)),
    ?TRACE(P, 1, Message),
    P ! {message, M, Message},
    loop(P, M).

loop(P) ->
    receive
        {message, M, Message} ->
            ?TRACE(P, M, Message),
            P ! {message, M, Message},
            loop(P);
        stop ->
            P ! stop
    end.

loop(P, 1) -> P ! stop;
loop(P, M) ->
    receive
        {message, M, Message} ->
            ?TRACE(P, M, Message),
            P ! {message, M-1, Message},
            loop(P, M-1)
    end.
