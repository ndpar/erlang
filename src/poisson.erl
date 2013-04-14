%%
%% http://www.computer.org/csdl/mags/cs/2012/06/mcs2012060008.html
%% http://steve.vinoski.net/pdf/CISE-Vinoski-Concurrency-and-Message-Passing-in-Erlang.pdf
%%

-module(poisson).

%% API
-export([poisson1d/0]).

%% Internal exports
-export([phi/4]).


poisson1d() ->
    NSites = 16,
    H = 0.1,
    NIters = 10,
    Collector = self(),
    Sentinel = spawn(fun sentinel/0),
    Pids = lists:foldl(
        fun(I, [Prev|_]=Acc) ->
            Rho = case NSites div 2 of
                      I -> 1;
                      _ -> 0
                  end,
            Args = [Collector, NIters, Prev, {I,H,Rho}],
            Pid = spawn(?MODULE, phi, Args),
            Prev ! {set_next, Pid},
            [Pid | Acc]
        end,
        [Sentinel],
        lists:seq(0, NSites-1)),
    hd(Pids) ! {set_next, Sentinel},
    lists:foreach(
        fun(Pid) ->
            receive
                {Pid, I, Phi} ->
                    io:format("~2w ~.10f~n", [I, Phi])
            end
        end,
        tl(lists:reverse(Pids))),
    Sentinel ! stop,
    ok.

phi(Collector, NIters, Prev, Consts) ->
    receive
        {set_next, Next} ->
            phi(NIters, Collector, Prev, Next, Consts, 0.0)
    end.

phi(0, Collector, _, _, {I, _, _}, Phi) ->
    Collector ! {self(), I, Phi};
phi(NIters, Collector, Prev, Next, {_, H, Rho}=Consts, Phi) ->
    PhiPrev = adjacent_phi(Prev, Phi),
    PhiNext = adjacent_phi(Next, Phi),
    NewPhi = (PhiPrev + PhiNext)/2 + H/2 * Rho,
    phi(NIters-1, Collector, Prev, Next, Consts, NewPhi).

adjacent_phi(Adjacent, Phi) ->
    Adjacent ! {put, Phi, self()},
    receive
        {put, AdjacentPhi, Adjacent} ->
            AdjacentPhi
    end.

sentinel() ->
    receive
        {set_next, _} ->
            ok;
        {put, _, Pid} ->
            Pid ! {put, 0.0, self()};
        stop ->
            exit(normal)
    end,
    sentinel().
