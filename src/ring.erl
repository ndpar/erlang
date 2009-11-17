%%
%% Solution for Exercise 4-2 from book "Erlang Programming", p.115
%% In the shell type:
%%
%% 1> ring:start(2, 5, message).
%%
-module(ring).
-export([start/3]).
-export([create/4]).

start(M, N, Message) ->
    create(undef, N, M, Message).

create(Parent, 0, M, Message) ->
    Parent ! {created, self()},
%    io:format("~w sent ~w to ~w~n", [self(), {created, self()}, Parent]),
    evaluate(Parent, M, Message);

create(Parent, N, M, Message) ->
    Child = spawn(?MODULE, create, [self(), N-1, M, Message]),
    io:format("~w ~w created~n", [Child, N]),
    evaluate(Parent, M, Message).

evaluate(undef, M, Message) ->
%    io:format("~w evaluating: parent=~w, M=~w~n", [self(), undef, M]),
    receive
        {created, Last} ->
            Last ! Message,
            io:format("~w sent ~w to ~w~n", [self(), Message, Last]),
            evaluate(Last, M-1, Message)
    end;

evaluate(Parent, 0, _) ->
%    io:format("~w evaluating: parent=~w, M=~w~n", [self(), Parent, 0]),
    receive
        Msg ->
            io:format("~w received ~w~n", [self(), Msg]),
            Parent ! stop,
            io:format("~w sent ~w to ~w~n", [self(), stop, Parent])
    end;

evaluate(Parent, M, Message) ->
%    io:format("~w evaluating: parent=~w, M=~w~n", [self(), Parent, M]),
    receive
        {created, Last} ->
%            io:format("~w received ~w~n", [self(), {created, Last}]),
            Parent ! {created, Last},
%            io:format("~w sent ~w to ~w~n", [self(), {created, Last}, Parent]),
            evaluate(Parent, M, Message);
        Message ->
            io:format("~w received ~w~n", [self(), Message]),
            Parent ! Message,
            io:format("~w sent ~w to ~w~n", [self(), Message, Parent]),
            evaluate(Parent, M-1, Message)
    end.

