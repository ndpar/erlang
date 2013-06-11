-module(customer).

-behaviour(gen_fsm).

%% API
-export([start/0, sit_down/1, done/1, wait/1, sorry/1]).

%% Callback functions
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

%% FSM states
-export([wait/2, being_served/2]).

-record(state, {waited, served}).

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
    error_logger:info_msg("~p: Good morning.~n", [self()]),
    {ok, wait, #state{waited = calendar:universal_time()}}.

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
%% States and transitions
%%====================================================================

wait(sit_down, #state{waited = Waited}) ->
    WaitingTime = calendar:time_difference(Waited, calendar:universal_time()),
    error_logger:info_msg("~p: I've been waiting for ~p.~n", [self(), WaitingTime]),
    NewStateData = #state{served = calendar:universal_time()},
    {next_state, being_served, NewStateData};
wait(wait, StateData) ->
    error_logger:info_msg("~p: OK.~n", [self()]),
    {next_state, wait, StateData};
wait(exit, _StateData) ->
    error_logger:info_msg("~p: Maybe tomorrow.~n", [self()]),
    {stop, normal, #state{}}.

being_served(exit, #state{served = Served}) ->
    ServedTime = calendar:time_difference(Served, calendar:universal_time()),
    error_logger:info_msg("~p: Thank you. It took ~p.~n", [self(), ServedTime]),
    {stop, normal, #state{}}.
