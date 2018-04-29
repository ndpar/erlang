%%
%% @doc Exercises from the book
%%
%% @reference J.Armstrong. <em>Programming Erlang</em>.
%%
-module(prog_erl).
-author("Andrey Paramonov").

-export([die_after/1, my_spawn/3]).
-export([my_spawn2/3]).
-export([my_spawn/4]).
-export([ch_13_4/0, keep_alive/2]).
-export([ch_13_5/1, keep_alive/1]).
-export([ch_13_6/0]).

%% =============================================================================
%% Chapter 13. Errors in Concurrent Programs
%% =============================================================================

%%
%% Exercise 1.
%% Write a function my_spawn(Mod, Func, Args) that behaves like spawn(Mod, Func,
%% Args) but with one difference. If the spawned process dies, a message should
%% be printed saying why the process died and how long the process lived for
%% before it died.
%%
%% > prog_erl:my_spawn(prog_erl, die_after, [5000]).
%%
-spec my_spawn(Module :: module(), Function :: atom(), Args :: [term()]) -> pid().

my_spawn(Mod, Func, Args) ->
  spawn(fun() ->
    Pid = spawn(Mod, Func, Args),
    Born = erlang:system_time(millisecond),
    Ref = monitor(process, Pid),
    receive
      {'DOWN', Ref, process, Pid, Why} ->
        Died = erlang:system_time(millisecond),
        io:format("~p lived for ~p ms, died with: ~p~n", [Pid, Died - Born, Why])
    end
        end).

die_after(Millis) ->
  io:format("~p is going to die in ~p ms~n", [self(), Millis]),
  receive
  after Millis -> 1 / 0
  end.

%%
%% Exercise 2.
%% Solve the previous exercise using the on_exit function shown earlier in this
%% chapter.
%%
%% > prog_erl:my_spawn2(prog_erl, die_after, [5000]).
%%

on_exit(Pid, Fun) ->
  spawn(fun() ->
    Ref = monitor(process, Pid),
    receive
      {'DOWN', Ref, process, Pid, Why} -> Fun(Why)
    end
        end).

my_spawn2(Mod, Func, Args) ->
  Pid = spawn(Mod, Func, Args),
  Born = erlang:system_time(millisecond),
  on_exit(Pid, fun(Why) ->
    Died = erlang:system_time(millisecond),
    io:format("~p lived for ~p ms, died with: ~p~n", [Pid, Died - Born, Why])
               end).

%%
%% Exercise 3.
%% Write a function my_spawn(Mod, Func, Args, Time) that behaves like spawn(Mod,
%% Func, Args) but with one difference. If the spawned process lives for more
%% than Time seconds, it should be killed.
%%
%% > prog_erl:my_spawn(prog_erl, die_after, [7000], 5).
%%
my_spawn(Mod, Func, Args, Time) ->
  spawn(fun() ->
    {Pid, Ref} = spawn_monitor(Mod, Func, Args),
    receive
      {'DOWN', Ref, process, Pid, Why} -> Why
    after Time * 1000 ->
      io:format("~p is about to die~n", [Pid]),
      exit(Pid, too_old)
    end
        end).

%%
%% Exercise 4.
%% Write a function that creates a registered process that writes out "I'm still
%% running" every five seconds. Write a function that monitors this process and
%% restarts it if it dies. Start the global process and the monitor process.
%% Kill the global process and check that it has been restarted by the monitor.
%%
%% > prog_erl:keep_alive(ch_13_4, fun prog_erl:ch_13_4/0).
%% > whereis(ch_13_4).
%% > exit(whereis(ch_13_4), die).
%% > whereis(ch_13_4).
%%
keep_alive(Name, Fun) ->
  register(Name, Pid = spawn(Fun)),
  on_exit(Pid, fun(_Why) -> keep_alive(Name, Fun) end).

ch_13_4() ->
  io:format("I'm still running~n"),
  receive
  after 5000 -> ch_13_4()
  end.

%%
%% Exercise 5.
%% Write a function that starts and monitors several worker processes. If any
%% of the worker processes dies abnormally, restart it.
%%
%% > prog_erl:ch_13_5([3000, 7000]).
%%
keep_alive(Fun) ->
  on_exit(spawn(Fun), fun(_Why) -> keep_alive(Fun) end).

ch_13_5([]) -> ok;
ch_13_5([M | Ms]) ->
  keep_alive(fun() -> die_after(M) end),
  ch_13_5(Ms).

%%
%% Exercise 6.
%% Write a function that starts and monitors several worker processes. If any
%% of the worker processes dies abnormally, kill all the worker processes and
%% restart them all.
%%
%% > prog_erl:ch_13_6().
%%
ch_13_6() ->
  ch_13_6([
    fun() -> die_after(3000) end,
    fun() -> die_after(5000) end,
    fun() -> die_after(7000) end
  ]).

ch_13_6(Funs) ->
  ch_13_6(Funs, [spawn_monitor(F) || F <- Funs]).

ch_13_6(Funs, Procs) ->
  receive
    {'DOWN', Ref, process, Pid, Why} ->
      io:format("~p died with: ~p~n", [Pid, Why]),
      lists:foreach(
        fun({P, R} = Proc) ->
          demonitor(R),
          io:format("Killing ~p~n", [Proc]),
          exit(P, kill_all)
        end,
        lists:delete({Pid, Ref}, Procs)),
      ch_13_6(Funs)
  end.