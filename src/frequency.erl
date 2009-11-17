%%
%% F.Cesarini & S.Thomson, Erlang Programming, p.121.
%% Client/Server Pattern.
%%
%%  1> frequency:start().
%%  2> frequency:allocate().
%%  ...
%%  8> frequency:allocate().
%%  9> frequency:deallocate(11).
%% 10> frequency:allocate().
%% 11> frequency:stop().
%%
-module(frequency).
-export([start/0, stop/0]).
-export([allocate/0, deallocate/1]).
-export([init/0]).

% Start function to create and initialize the server

start() ->
    register(frequency, spawn(frequency, init, [])).

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
            NewFrequencies = deallocate(Frequencies, Freq),
            reply(Pid, ok),
            loop(NewFrequencies);
        {request, Pid, stop} ->
            reply(Pid, ok)
    end.

allocate({[], Allocated}, _Pid) ->
    {{[], Allocated}, {error, no_frequency}};
allocate({[Freq|Free], Allocated}, Pid) ->
    {{Free, [{Freq,Pid}|Allocated]}, {ok, Freq}}.

deallocate({Free,Allocated}, Freq) ->
    NewAllocated = lists:keydelete(Freq, 1, Allocated),
    {[Freq|Free], NewAllocated}.

reply(Pid, Reply) ->
    Pid ! {reply, Reply}.


% Client functions

stop()           -> call(stop).
allocate()       -> call(allocate).
deallocate(Freq) -> call({deallocate, Freq}).

call(Message) ->
    frequency ! {request, self(), Message},
    receive
        {reply, Reply} -> Reply
    end.