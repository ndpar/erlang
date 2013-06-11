-module(customer).
-author("Andrey Paramonov <github@ndpar.com>").
-behaviour(gen_fsm).

%% API
-export([start/0, sit_down/1, done/1, wait/1, sorry/1]).

%% Callback functions
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

%% FSM states
-export([waiting/2, served/2]).

%%====================================================================
%% API
%%====================================================================

start() ->
    {ok,Pid} = gen_fsm:start(?MODULE, [], []),
    Pid.

sit_down(Customer) ->
    gen_fsm:send_event(Customer, sit_down).

done(Customer) ->
    gen_fsm:send_event(Customer, exit).

wait(Customer) ->
    gen_fsm:send_event(Customer, wait).

sorry(Customer) ->
    gen_fsm:send_event(Customer, exit).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================

init([]) ->
    log("Good morning."),
    {ok, waiting, unow()}.

handle_info(_Info, _StateName, StateData) ->
    {stop, undefined, StateData}.

handle_event(_Event, _StateName, StateData) ->
    {stop, undefined, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop, undefined, none, StateData}.

terminate(_Reason, _StateName, _StateData) ->
    ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%====================================================================
%% FSM states and transitions
%%====================================================================

waiting(sit_down, WaitStart) ->
    log("I've been waiting for ~p sec.", [duration(WaitStart)]),
    {next_state, served, unow()};

waiting(wait, StateData) ->
    log("OK."),
    {next_state, waiting, StateData};

waiting(exit, _StateData) ->
    log("Maybe tomorrow."),
    {stop, normal, undefined}.

served(exit, ServiceStart) ->
    log("Thank you. The haircut took ~p sec.", [duration(ServiceStart)]),
    {stop, normal, undefined}.

%%====================================================================
%% Helper functions
%%====================================================================

log(Message) ->
    log(Message, []).

log(Message, Args) ->
    error_logger:info_msg("~p: " ++ Message ++ "~n", [self() | Args]).

unow() ->
    calendar:universal_time().

duration(Start) ->
    {_, {_, _, Seconds}} = calendar:time_difference(Start, unow()),
    Seconds.
