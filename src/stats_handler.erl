%%
%% Erlang Programming, p.138.
%% Exercise 5-4: Event Statistics
%%
%% 1> event_manager:start(alarm, [{stats_handler, []}]).
%% ok
%% 2> event_manager:send_event(alarm, {event, 156, link_up}).
%% ok
%% 3> event_manager:send_event(alarm, {raise_alarm, 10, cabinet_open}).
%% ok
%% 4> event_manager:send_event(alarm, {clear_alarm, 10, cabinet_open}).
%% ok
%% 5> event_manager:send_event(alarm, {event, 156, link_up}).
%% ok
%% 6> event_manager:get_data(alarm, stats_handler).
%% {data,[{{clear_alarm,10},1},
%%        {{raise_alarm,10},1},
%%        {{event,156},2}]}
%% 7> event_manager:stop(alarm).
%%
-module(stats_handler).
-export([init/1, terminate/1, handle_event/2]).

init(InitStats) -> InitStats.

terminate(Stats) -> {data, Stats}.

handle_event({Type, Id, _Description}, Stats) ->
    case lists:keysearch({Type, Id}, 1, Stats) of
        false ->
            [{{Type, Id}, 1}|Stats];
        {value, {{Type, Id}, Count}} ->
            lists:keyreplace({Type, Id}, 1, Stats, {{Type, Id}, Count + 1})
    end.
