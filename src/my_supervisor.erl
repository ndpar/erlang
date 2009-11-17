%%
%% F.Cesarini & S.Thomson, Erlang Programming, p.153.
%% Supervisor Pattern.
%%
%% 1> my_supervisor:start_link(my_supervisor, [{add_two, start, []}]).
%% ok
%% 2> whereis(add_two).
%% <0.34.0>
%% 3> exit(whereis(add_two), kill).
%% true
%% 4> add_two:request(100).
%% 102
%% 5> whereis(add_two).
%% <0.37.0>
%% 6> my_supervisor:stop(my_supervisor).
%% ok
%% 7> whereis(add_two).
%% undefined
%%
-module(my_supervisor).
-export([start_link/2, stop/1]).
-export([init/1]).

start_link(Name, ChildSpecList) ->
    register(Name, spawn_link(my_supervisor, init, [ChildSpecList])), ok.

init(ChildSpecList) ->
    process_flag(trap_exit, true),
    loop(start_children(ChildSpecList)).

start_children([]) -> [];
start_children([{M,F,A} | ChildSpecList]) ->
    case (catch apply(M, F, A)) of
        {ok, Pid} ->
            [{Pid, {M,F,A}} | start_children(ChildSpecList)];
        _ ->
            start_children(ChildSpecList)
    end.

loop(ChildList) ->
    receive
        {'EXIT', Pid, _Reason} ->
            NewChildList = restart_child(Pid, ChildList),
            loop(NewChildList);
        {stop, From} ->
            From ! {reply, terminate(ChildList)}
    end.

restart_child(Pid, ChildList) ->
    {value, {Pid, {M,F,A}}} = lists:keysearch(Pid, 1, ChildList),
    {ok, NewPid} = apply(M,F,A),
    [{NewPid, {M,F,A}} | lists:keydelete(Pid, 1, ChildList)].

terminate([{Pid, _} | ChildList]) ->
    exit(Pid, kill),
    terminate(ChildList);
terminate(_ChildList) -> ok.

stop(Name) ->
    Name ! {stop, self()},
    receive {reply, Reply} -> Reply end.
