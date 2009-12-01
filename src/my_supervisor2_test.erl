-module(my_supervisor2_test).
-export([test1/0, test2/0, test3/0, test4/0]).
-export([test5/0, test6/0]).

% 1> my_supervisor2_test:test1().
% Spawned permanent process <0.34.0>
% Killing process <0.34.0>
% Where is our process? <0.35.0>
% ok
test1() ->
    my_supervisor2:start_link(my_supervisor2, [{echo3, start, [], permanent, 5}]),
    receive after 1000 -> ok end,
    io:format("Spawned permanent process ~p~n", [whereis(echo3)]),
    io:format("Killing process ~p~n", [whereis(echo3)]),
    exit(whereis(echo3), kill),
    receive after 1000 -> ok end,
    io:format("Where is our process? ~p~n", [whereis(echo3)]),
    my_supervisor2:stop(my_supervisor2).

% 1> my_supervisor2_test:test2().
% Spawned permanent process <0.40.0>
% Exiting process <0.40.0>
% Where is our process? <0.41.0>
% ok
test2() ->
    my_supervisor2:start_link(my_supervisor2, [{echo3, start, [], permanent, 5}]),
    receive after 1000 -> ok end,
    io:format("Spawned permanent process ~p~n", [whereis(echo3)]),
    io:format("Exiting process ~p~n", [whereis(echo3)]),
    echo3 ! stop,
    receive after 1000 -> ok end,
    io:format("Where is our process? ~p~n", [whereis(echo3)]),
    my_supervisor2:stop(my_supervisor2).

% 1> my_supervisor2_test:test3().
% Spawned transient process <0.34.0>
% Killing process <0.34.0>
% Where is our process? <0.35.0>
% ok
test3() ->
    my_supervisor2:start_link(my_supervisor2, [{echo3, start, [], transient, 5}]),
    receive after 1000 -> ok end,
    io:format("Spawned transient process ~p~n", [whereis(echo3)]),
    io:format("Killing process ~p~n", [whereis(echo3)]),
    exit(whereis(echo3), kill),
    receive after 1000 -> ok end,
    io:format("Where is our process? ~p~n", [whereis(echo3)]),
    my_supervisor2:stop(my_supervisor2).

% 1> my_supervisor2_test:test4().
% Spawned transient process <0.40.0>
% Exiting process <0.40.0>
% Where is our process? undefined
% ok
test4() ->
    my_supervisor2:start_link(my_supervisor2, [{echo3, start, [], transient, 5}]),
    receive after 1000 -> ok end,
    io:format("Spawned transient process ~p~n", [whereis(echo3)]),
    io:format("Exiting process ~p~n", [whereis(echo3)]),
    echo3 ! stop,
    receive after 1000 -> ok end,
    io:format("Where is our process? ~p~n", [whereis(echo3)]),
    my_supervisor2:stop(my_supervisor2).

% 1> my_supervisor2_test:test5().
% Spawned permanent process <0.49.0>
% Exiting process <0.49.0>
% Where is our process? <0.50.0>
% Spawned permanent process <0.50.0>
% Exiting process <0.50.0>
% Where is our process? <0.51.0>
% Spawned permanent process <0.51.0>
% Exiting process <0.51.0>
% Where is our process? <0.52.0>
% Spawned permanent process <0.52.0>
% Exiting process <0.52.0>
% Where is our process? <0.53.0>
% Spawned permanent process <0.53.0>
% Exiting process <0.53.0>
% Where is our process? <0.54.0>
% Spawned permanent process <0.54.0>
% Exiting process <0.54.0>
% Process <0.54.0> reached restart threshold
% ok
test5() ->
    my_supervisor2:start_link(my_supervisor2, [{echo3, start, [], permanent, 5}]),
    receive after 1000 -> ok end,
    test5_loop(),
    my_supervisor2:stop(my_supervisor2).

test5_loop() ->
    io:format("Spawned permanent process ~p~n", [whereis(echo3)]),
    io:format("Exiting process ~p~n", [whereis(echo3)]),
    echo3 ! stop,
    receive after 1000 -> ok end,
    case whereis(echo3) of
        undefined -> ok;
        Pid ->
            io:format("Where is our process? ~p~n", [Pid]),
            test5_loop()
    end.

% 1> my_supervisor2_test:test6().
% Starting {echo3,start,[]}
% Spawned process {<0.35.0>,0,{echo3,start,[],transient,5}}
% Stopping process <0.35.0>
% Where is our process? undefined
% ok
test6() ->
    my_supervisor2:start_link(my_supervisor2, []),
    io:format("Starting ~p~n", [{echo3, start, []}]),
    {Pid, Id, Spec} = my_supervisor2:start_child(my_supervisor2, echo3, start, []),
    io:format("Spawned process ~p~n", [{Pid, Id, Spec}]),
    io:format("Stopping process ~p~n", [whereis(echo3)]),
    my_supervisor2:stop_child(my_supervisor2, Id),
    receive after 1000 -> ok end,
    io:format("Where is our process? ~p~n", [whereis(echo3)]),
    my_supervisor2:stop(my_supervisor2).
