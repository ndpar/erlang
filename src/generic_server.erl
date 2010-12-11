%% How to use:
%%
%% generic_server:start().
%% generic_server:request(5). => 7
%%
%% generic_server:change_state(10).
%% generic_server:request(5). => 15
%%
%% generic_server:change_function( fun(X,Y) -> X * Y end ).
%% generic_server:request(5). => 50

-module(generic_server).

-export([start/0, loop/2]).
-export([request/1, change_state/1, change_function/1]).

%% Server

start() ->
    register(?MODULE, spawn(?MODULE, loop, [2, fun erlang:'+'/2])).

loop(State, Function) ->
    receive
        {new_state, S} -> loop(S, Function);
        {new_fun, F} -> loop(State, F);
        {request, From, Request} ->
            From ! {response, apply(Function, [State, Request])},
            loop(State, Function)
    end.

%% Client

change_state(NewState) ->
    ?MODULE ! {new_state, NewState},
    ok.

change_function(NewFunction) ->
    ?MODULE ! {new_fun, NewFunction},
    ok.

request(Request) ->
    ?MODULE ! {request, self(), Request},
    receive
        {response, Result} -> Result
    end.

