%%
%% In the shell type:
%%
%% 1> register(list_server, spawn(list_server, start, [])).
%% 2> list_server ! {self(), sort, [9,8,7,6,5,4,3,2,1]}.
%% 3> flush().
%%
-module(list_server).
-export([start/0]).

start() ->
    receive
        {From, sort, List} ->
            From ! lists:sort(List),
            start();
        stop ->
            true
%
% Uncomment this code to fix memory leakage
%
%        _Msg ->
%            io:format("Received message: ~w~n", [_Msg]),
%            start()
    end.