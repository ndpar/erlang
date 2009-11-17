%%
%% F.Cesarini & S.Thomson, Erlang Programming, p.150.
%% Reliable Client/Server
%%
%% Based on frequency2.erl
%%
%% Test 1: Kill the client
%%
%% 1> frequency_reliable:start().
%% ok
%% 2> frequency_reliable:allocate().
%% {ok,10}
%% 3> frequency_reliable:allocate().
%% {ok,11}
%% 4> exit(self(), kill).
%% ** exception exit: killed
%% 5> frequency_reliable:allocate().
%% {ok,10}
%% 6> frequency_reliable:allocate().
%% {ok,11}
%%
%% Test 2: Kill the server
%%
%% 1> frequency_reliable:start().
%% ok
%% 2> frequency_reliable:allocate().
%% {ok,10}
%% 3> self().
%% <0.31.0>
%% 4> exit(whereis(frequency_reliable), kill).
%% true
%% 5> self().
%% <0.37.0>
%%
-module(frequency_reliable).
-export([start/0, stop/0]).
-export([allocate/0, deallocate/1]).
-export([init/0]).

% Start function to create and initialize the server

start() ->
    register(frequency_reliable, spawn(frequency_reliable, init, [])), ok.

init() ->
    process_flag(trap_exit, true),
    Frequencies = {get_frequencies(), []},
    loop(Frequencies).

get_frequencies() -> [10,11,12,13,14,15].

loop(Frequencies) ->
    receive
        {request, Pid, allocate} ->
            {NewFrequencies, Reply} = allocate(Frequencies, Pid),
            reply(Pid, Reply),
            loop(NewFrequencies);
        {request, Pid, {deallocate, Freq}} ->
            {NewFrequencies, Reply} = deallocate(Frequencies, Freq, Pid),
            reply(Pid, Reply),
            loop(NewFrequencies);
        {request, Pid, stop} ->
            {_, Allocated} = Frequencies,
            case Allocated of
                [] ->
                    reply(Pid, ok);
                _ ->
                    reply(Pid, {error, frequencies_in_use}),
                    loop(Frequencies)
            end;
        {'EXIT', Pid, _Reason} ->
            NewFrequencies = exited(Frequencies, Pid),
            loop(NewFrequencies)
    end.

exited({Free, Allocated}, Pid) ->
    case lists:keysearch(Pid, 2, Allocated) of
        {value, {Freq, Pid}} ->
            NewAllocated = lists:keydelete(Freq, 1, Allocated),
            exited({[Freq|Free], NewAllocated}, Pid);
        false ->
            {Free, Allocated}
    end.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};

allocate({[Freq|Free], Allocated}, Pid) ->
    ClientFrequencies = my_lists:keyfilter(Pid, 2, Allocated),
    case length(ClientFrequencies) of
        3 -> {{[Freq|Free], Allocated}, {error, exceed_limit}};
        _ ->
            link(Pid),
            {{Free, [{Freq,Pid}|Allocated]}, {ok, Freq}}
    end.

deallocate({Free, Allocated}, Freq, Pid) ->
    case lists:member({Freq, Pid}, Allocated) of
        true ->
            unlink(Pid),
            NewAllocated = lists:keydelete(Freq, 1, Allocated),
            {{[Freq|Free], NewAllocated}, ok};
        _ ->
            {{Free, Allocated}, {error, foreign_frequency}}
    end.

reply(Pid, Reply) ->
    Pid ! {reply, Reply}.

% Client functions

stop()           -> call(stop).
allocate()       -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).

call(Message) ->
    frequency_reliable ! {request, self(), Message},
    receive {reply, Reply} -> Reply end.
