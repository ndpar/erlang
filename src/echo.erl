%%
%% In the shell type: echo:go().
%%
-module(echo).
-export([go/0, loop/0]).

go() ->
    io:format("~w Hi~n", [self()]),

    Pid = spawn(echo, loop, []),
    io:format("~w Spawned process ~w~n", [self(), Pid]),

    Pid ! {self(), hello},
    io:format("~w Sent hallo to ~w~n", [self(), Pid]),

    io:format("~w Start listening for incoming messages~n", [self()]),
    receive
        {Pid, Msg} ->
            io:format("~w Received ~w from ~w~n", [self(), Msg, Pid])
    end,
    Pid ! stop,
    io:format("~w Sent stop~n", [self()]).

loop() ->
    io:format("~w Hi~n", [self()]),
    io:format("~w Start listening for incoming messages~n", [self()]),
    receive
        {From, Msg} ->
            io:format("~w Received ~w from ~w~n", [self(), Msg, From]),
            From ! {self(), Msg},
            io:format("~w Sent ~w back to ~w~n", [self(), Msg, From]),
            loop();
        stop ->
            io:format("~w Received stop~n", [self()]),
            true
    end.