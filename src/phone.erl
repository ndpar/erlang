%%
%% Erlang Programming, p.138.
%% Exercise 5-5: Phone FSM
%%
%% 1> phone:start().
%%
%% 2> phone:incoming("555-111-2222").
%% 3> phone:other_on_hook("555-111-2222").
%% log incoming call
%%
%% 4> phone:incoming("555-333-4444").
%% 5> phone:off_hook("555-333-4444").
%% 6> phone:on_hook("555-333-4444").
%% log incoming call and its duration
%%
%% 7> phone:off_hook().
%% 8> phone:on_hook().
%% no log entry
%%
%% 9> phone:off_hook().
%% 10> phone:connect("555-555-6666").
%% 11> phone:on_hook("555-555-6666").
%% log outcoming call and its duration
%%
%% 12> phone:stop().
%%
-module(phone).
-export([start/0, stop/0]).
-export([incoming/1, off_hook/0, off_hook/1, other_on_hook/1, on_hook/0, on_hook/1, connect/1]).
-export([init/0]).

start() ->
    event_manager:start(billing, [{log_handler, "phone.log"}]),
    register(phone, spawn(?MODULE, init, [])),
    ok.

stop() ->
    event_manager:stop(billing),
    ok.

% Initial state

init() -> idle().

% Events

incoming(Number) -> phone ! {incoming, Number}.
off_hook() -> phone ! off_hook.
off_hook(Number) -> phone ! {off_hook, Number}.
other_on_hook(Number) -> phone ! {other_on_hook, Number}.
on_hook() -> phone ! on_hook.
on_hook(Number) -> phone ! {on_hook, Number}.
connect(Number) -> phone ! {other_off_hook, Number}.

% States and transitions

idle() ->
    receive
        {incoming, Number} ->
            start_ringing(Number),
            ringing(Number);
        off_hook ->
            start_tone(),
            dial()
    end.

ringing(Number) ->
    receive
        {other_on_hook, Number} ->
            stop_ringing(),
            idle();
        {off_hook, Number} ->
            stop_ringing(Number),
            connected(Number)
    end.

connected(Number) ->
    receive
        {on_hook, Number} ->
            stop_conversation(Number),
            idle()
    end.

dial() ->
    receive
        on_hook ->
            stop_tone(),
            idle();
        {other_off_hook, Number} ->
            start_conversation(Number),
            connected(Number)
    end.

% Actions

start_ringing(Number) -> event_manager:send_event(billing, {incoming, 10, Number}).
stop_ringing() -> ok.
stop_ringing(Number) -> event_manager:send_event(billing, {accept_incoming, 20, Number}).
start_tone() -> ok.
stop_tone() -> ok.
start_conversation(Number) -> event_manager:send_event(billing, {start_outgoing, 20, Number}).
stop_conversation(Number) -> event_manager:send_event(billing, {stop_conversation, 30, Number}).
