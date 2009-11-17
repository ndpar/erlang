%%
%% F.Cesarini & S.Thomson, Erlang Programming, p.138.
%% Exercise 5-2
%%
%% 1> frequency2:start().
%% ok
%% 2> frequency2:allocate().
%% {ok,10}
%% 3> frequency2:allocate().
%% {ok,11}
%% 4> frequency2:allocate().
%% {ok,12}
%% 5> frequency2:allocate().
%% {error,exceed_limit}
%% 6> frequency2:deallocate(13).
%% {error,foreign_frequency}
%% 7> frequency2:deallocate(11).
%% ok
%% 8> frequency2:stop().
%% {error,frequencies_in_use}
%% 9> frequency2:deallocate(10).
%% ok
%% 10> frequency2:deallocate(12).
%% ok
%% 11> frequency2:stop().
%% ok
%%
-module(frequency2).
-export([start/0, stop/0]).
-export([allocate/0, deallocate/1]).
-export([init/0]).

% Start function to create and initialize the server

start() ->
    register(frequency2, spawn(frequency2, init, [])), ok.

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
    frequency2 ! {request, self(), Message},
    receive {reply, Reply} -> Reply end.
