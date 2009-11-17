%%
%% F.Cesarini & S.Thomson, Erlang Programming, p.150.
%% Reliable Client/Server
%%
%% Based on frequency2.erl
%%
-module(frequency_reliable).
-export([start/0, stop/0]).
-export([allocate/0, deallocate/1]).
-export([init/0]).

% Start function to create and initialize the server

start() ->
    register(frequency_reliable, spawn(frequency_reliable, init, [])), ok.

init() ->
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
            end
    end.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};

allocate({[Freq|Free], Allocated}, Pid) ->
    ClientFrequencies = my_lists:keyfilter(Pid, 2, Allocated),
    case length(ClientFrequencies) of
        3 -> {{[Freq|Free], Allocated}, {error, exceed_limit}};
        _ -> {{Free, [{Freq,Pid}|Allocated]}, {ok, Freq}}
    end.

deallocate({Free, Allocated}, Freq, Pid) ->
    case lists:member({Freq, Pid}, Allocated) of
        true ->
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
