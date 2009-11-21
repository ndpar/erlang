%%
%% F.Cesarini & S.Thomson, Erlang Programming, p.155.
%% Exercise 6-3: Supervisor Process
%%
-module(my_supervisor2).
-export([start_link/2, stop/1]).
-export([init/1]).

start_link(Name, ChildSpecList) ->
    register(Name, spawn_link(my_supervisor2, init, [ChildSpecList])), ok.

init(ChildSpecList) ->
    process_flag(trap_exit, true),
    loop(start_children(ChildSpecList)).

start_children([]) -> [];
start_children([{M,F,A,T} | ChildSpecList]) ->
    case (catch apply(M, F, A)) of
        {ok, Pid} ->
            [{Pid, {M,F,A,T}} | start_children(ChildSpecList)];
        _ ->
            start_children(ChildSpecList)
    end.

loop(ChildList) ->
    receive
        {'EXIT', Pid, normal} ->
            NewChildList = restart_permanent_child(Pid, ChildList),
            loop(NewChildList);
        {'EXIT', Pid, _Reason} ->
            NewChildList = restart_child(Pid, ChildList),
            loop(NewChildList);
        {stop, From} ->
            From ! {reply, terminate(ChildList)}
    end.

restart_permanent_child(Pid, ChildList) ->
    case lists:keysearch(Pid, 1, ChildList) of
        {value, {Pid, {M,F,A,permanent}}} ->
            {ok, NewPid} = apply(M,F,A),
            [{NewPid, {M,F,A,permanent}} | lists:keydelete(Pid, 1, ChildList)];
        _ ->
            lists:keydelete(Pid, 1, ChildList)
    end.

restart_child(Pid, ChildList) ->
    {value, {Pid, {M,F,A,T}}} = lists:keysearch(Pid, 1, ChildList),
    {ok, NewPid} = apply(M,F,A),
    [{NewPid, {M,F,A,T}} | lists:keydelete(Pid, 1, ChildList)].

terminate([{Pid, _} | ChildList]) ->
    exit(Pid, kill),
    terminate(ChildList);
terminate(_ChildList) -> ok.

stop(Name) ->
    Name ! {stop, self()},
    receive {reply, Reply} -> Reply end.
