%%
%% In the shell type:
%%
%% 1> timer:tc(myring, start, [100000]).
%% 2> timer:tc(myring, start, [1000000]).
%% 3> timer:tc(myring, start, [10000000]).
%%
-module(myring).
-export([start/1, start_proc/2]).

start(Num) ->
    start_proc(Num, self()).

start_proc(0, Pid) ->
    Pid ! ok;

start_proc(Num, Pid) ->
    NPid = spawn(?MODULE, start_proc, [Num-1, Pid]),
    NPid ! ok,
    receive ok -> ok end.