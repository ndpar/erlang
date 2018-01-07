%
% J. Armstrong, Programming Erlang, 2nd Edition
% Exercise 12.3, p. 198.
%
% Compile with trace disabled:
% 1> c(ring2, {d, no_trace}).
%
% Run from the shell:
% $ time erl -noshell -pa ebin -eval 'ring2:start(200000, 2000000, hello)' -s init stop
%
-module(ring2).
-export([start/3]).

-ifndef(no_trace).
-define(TRACE(P, M, Message), io:format("~p -> ~p: ~p (~p)~n", [self(), P, Message, M])).
-else.
-define(TRACE(P, M, Message), void).
-endif.

%
% Creates N processes in a ring.
% Sends a message round the ring M times
% so that a total of N*M messages get sent.
%
start(N, M, Message) when N > 1 -> spawn(fun() -> ring(N-1, M, Message) end).

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
