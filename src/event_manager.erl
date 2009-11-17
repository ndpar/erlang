%%
%% Erlang Programming, p.133.
%% Event Manager Pattern.
%%
%% 1> event_manager:start(alarm, [{log_handler, "alarm.log"}]).
%% 2> event_manager:send_event(alarm, {raise_alarm, 10, cabinet_open}).
%% 3> event_manager:add_handler(alarm, io_handler, 1).
%% 4> event_manager:send_event(alarm, {clear_alarm, 10, cabinet_open}).
%% 5> event_manager:send_event(alarm, {event, 156, link_up}).
%% 6> event_manager:get_data(alarm, io_handler).
%% 7> event_manager:delete_handler(alarm, stats_handler).
%% 8> event_manager:stop(alarm).
%%
%% See also io_handler.erl, log_handler.erl
%%
-module(event_manager).
-export([start/2, stop/1]).
-export([add_handler/3, delete_handler/2, get_data/2, send_event/2]).
-export([swap_handlers/3]).
-export([init/1]).

% Start/stop functions

start(Name, HandlerList) ->
    register(Name, spawn(?MODULE, init, [HandlerList])), ok.

stop(Name) ->
    Name ! {stop, self()},
    receive {reply, Reply} -> Reply end.

init(HandlerList) ->
    loop(initialize(HandlerList)).

initialize([]) -> [];
initialize([{Handler, InitData}|Rest]) ->
    [{Handler, Handler:init(InitData)}|initialize(Rest)].

% Interface

add_handler(Name, Handler, InitData) ->
    call(Name, {add_handler, Handler, InitData}).

delete_handler(Name, Handler) ->
    call(Name, {delete_handler, Handler}).

get_data(Name, Handler) ->
    call(Name, {get_data, Handler}).

send_event(Name, Event) ->
    call(Name, {send_event, Event}).

% Main loop

call(Name, Msg) ->
    Name ! {request, self(), Msg},
    receive {reply, Reply} -> Reply end.

loop(State) ->
    receive
        {request, From, Msg} ->
            {Reply, NewState} = handle_msg(Msg, State),
            reply(From, Reply),
            loop(NewState);
        {stop, From} ->
            reply(From, terminate(State))
    end.

reply(To, Msg) ->
    To ! {reply, Msg}.

handle_msg({add_handler, Handler, InitData}, State) ->
    case lists:keysearch(Handler, 1, State) of
        false ->
            {ok, [{Handler, Handler:init(InitData)}|State]};
        _ ->
            {{error, already_registered}, State}
    end;

handle_msg({delete_handler, Handler}, State) ->
    case lists:keysearch(Handler, 1, State) of
        false ->
            {{error, instance}, State};
        {value, {Handler, Data}} ->
            Reply = {data, Handler:terminate(Data)},
            NewState = lists:keydelete(Handler, 1, State),
            {Reply, NewState}
    end;

%% I'm not sure how it's supposed to work
%% so I change the type of NewHandler to file name
%% (see the next method)
%%
%handle_msg({swap_handlers, OldHandler, NewHandler}, State) ->
%    case lists:keysearch(OldHandler, 1, State) of
%        false ->
%            {{error, instance}, State};
%        {value, {OldHandler, Data}} ->
%            NewInitData = OldHandler:terminate(Data),
%            NewData = NewHandler:init(NewInitData),
%            Reply = {data, NewData},
%            NewState = lists:keyreplace(OldHandler, 1, State, {NewHandler, NewData}),
%            {Reply, NewState}
%    end;

handle_msg({swap_handlers, log_handler, NewFileName}, State) ->
    case lists:keysearch(log_handler, 1, State) of
        false ->
            {{error, instance}, State};
        {value, {log_handler, Fd}} ->
            log_handler:terminate(Fd),
            NewFd = log_handler:init(NewFileName),
            Reply = {data, NewFd},
            NewState = lists:keyreplace(log_handler, 1, State, {log_handler, NewFd}),
            {Reply, NewState}
    end;

handle_msg({get_data, Handler}, State) ->
    case lists:keysearch(Handler, 1, State) of
        false ->
            {{error, instance}, State};
        {value, {Handler, Data}} ->
            {{data, Data}, State}
    end;

handle_msg({send_event, Event}, State) ->
    {ok, event(Event, State)}.

event(_Event, []) -> [];
event(Event, [{Handler, Data}|Rest]) ->
    [{Handler, Handler:handle_event(Event, Data)}|event(Event, Rest)].

terminate([]) -> [];
terminate([{Handler, Data}|Rest]) ->
    [{Handler, Handler:terminate(Data)}|terminate(Rest)].

% Event Handler interface:
%
% init(InitData)
% terminate(Data)
% handle_event(Event, Data)


%% Exercise 5-3: Swapping Handlers
%%
%% 1> event_manager:start(alarm, [{log_handler, "alarm.log"}]).
%% 2> event_manager:send_event(alarm, {event, 156, go_to_alarm}).
%% 3> event_manager:swap_handlers(alarm, log_handler, "server.log").
%% 4> event_manager:send_event(alarm, {event, 166, go_to_server}).
%% 5> event_manager:stop(alarm).

swap_handlers(Name, OldHandler, NewHandler) ->
    call(Name, {swap_handlers, OldHandler, NewHandler}).
