%%
%% @doc Ring benchmark.
%%
%% The shell process is the first in the ring.
%% It orchestrates the ring by sending internal
%% messages as well as the given message.
%% ```
%% 1> ring:start(2, 5, message).'''
%%
%% @reference [CT1] Exercise 4-2, p.115
%% @see ring2
%%
-module(ring).
-export([start/3]).
-export([create/4]).

-ifndef(no_trace).
-define(TRACE(P, M, Message), io:format("~p -> ~p: ~p (~p)~n", [self(), P, Message, M])).
-else.
-define(TRACE(P, M, Message), void).
-endif.

start(N, M, Message) ->
  create(undef, N - 1, M, Message).

create(Parent, 0, M, Message) ->
  Parent ! {created, self()},
  evaluate(Parent, M, Message);

create(Parent, N, M, Message) ->
  spawn(?MODULE, create, [self(), N - 1, M, Message]), % ignore PID
  evaluate(Parent, M, Message).

evaluate(undef, M, Message) ->
  receive
    {created, Shell} ->
      Shell ! Message,
      ?TRACE(Shell, M, Message),
      evaluate(Shell, M - 1, Message)
  end;

evaluate(Parent, 0, _) ->
  receive
    _ -> Parent ! stop
  end;

evaluate(Parent, M, Message) ->
  receive
    {created, Shell} ->
      Parent ! {created, Shell},
      evaluate(Parent, M, Message);
    Message ->
      Parent ! Message,
      ?TRACE(Parent, M, Message),
      evaluate(Parent, M - 1, Message)
  end.
