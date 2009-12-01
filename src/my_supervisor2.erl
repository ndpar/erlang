%%
%% F.Cesarini & S.Thomson, Erlang Programming, p.155.
%% Exercise 6-3: Supervisor Process
%%
-module(my_supervisor2).
-export([start_link/2, stop/1]).
-export([start_child/4, stop_child/2]).
-export([init/1]).

start_link(Name, ChildSpecList) ->
    start_id_generator(),
    register(Name, spawn_link(my_supervisor2, init, [ChildSpecList])), ok.

start_id_generator() ->
    case whereis(id_generator) of
        undefined -> id_generator:start();
        _ -> ok
    end.

init(ChildSpecList) ->
    process_flag(trap_exit, true),
    loop(start_children(ChildSpecList)).

start_children([]) -> [];
start_children([{M,F,A,T,C} | ChildSpecList]) ->
    Id = id_generator:next(),
    case (catch apply(M, F, A)) of
        {ok, Pid} ->
            io:format("~p started; id=~p; spec=~p~n", [Pid, Id, {M,F,A,T,C}]),
            [{Pid, Id, {M,F,A,T,C}} | start_children(ChildSpecList)];
        _ ->
            io:format("Skipped spec=~p~n", [{M,F,A,T,C}]),
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
        {start_child, From, {Module, Function, Arguments}} ->
            NewChildList = start_transient_child({Module, Function, Arguments}, ChildList),
            [NewChild | _Rest] = NewChildList,
            From ! {reply, NewChild},
            loop(NewChildList);
        {stop_child, From, Id} ->
            NewChildList = stop_transient_child(Id, ChildList),
            From ! {reply, ok},
            loop(NewChildList);
        {stop, From} ->
            From ! {reply, terminate(ChildList)}
    end.

start_transient_child({M, F, A}, ChildList) ->
    Id = id_generator:next(),
    case (catch apply(M, F, A)) of
        {ok, Pid} ->
            [{Pid, Id, {M, F, A, transient, 5}} | ChildList];
        _ ->
            ChildList
    end.

stop_transient_child(Id, ChildList) ->
    case lists:keysearch(Id, 2, ChildList) of
        {value, {Pid, Id, {_M, _F, _A, _T, _C}}} ->
            unlink(Pid),
            exit(Pid, kill),
            lists:keydelete(Id, 2, ChildList);
        _ ->
            ChildList
    end.

restart_permanent_child(Pid, ChildList) ->
    case lists:keysearch(Pid, 1, ChildList) of
        {value, {Pid, _Id, {_M,_F,_A,_T,0}}} ->
            io:format("Process ~p reached restart threshold~n", [Pid]),
            lists:keydelete(Pid, 1, ChildList);
        {value, {Pid, Id, {M,F,A,permanent,C}}} ->
            {ok, NewPid} = apply(M,F,A),
            [{NewPid, Id, {M,F,A,permanent,C-1}} | lists:keydelete(Pid, 1, ChildList)];
        _ ->
            lists:keydelete(Pid, 1, ChildList)
    end.

restart_child(Pid, ChildList) ->
    case lists:keysearch(Pid, 1, ChildList) of
        {value, {Pid, _Id, {_M,_F,_A,_T,0}}} ->
            io:format("Process ~p reached restart threshold~n", [Pid]),
            lists:keydelete(Pid, 1, ChildList);
        {value, {Pid, Id, {M,F,A,T,C}}} ->
            {ok, NewPid} = apply(M,F,A),
            [{NewPid, Id, {M,F,A,T,C-1}} | lists:keydelete(Pid, 1, ChildList)]
    end.

terminate([{Pid, _, _} | ChildList]) ->
    exit(Pid, kill),
    terminate(ChildList);
terminate(_ChildList) -> ok.

stop(Name) ->
    Name ! {stop, self()},
    receive {reply, Reply} -> Reply end.

start_child(Name, Module, Function, Arguments) ->
    Name ! {start_child, self(), {Module, Function, Arguments}},
    receive {reply, Reply} -> Reply end.

stop_child(Name, Id) ->
    Name ! {stop_child, self(), Id},
    receive {reply, Reply} -> Reply end.
