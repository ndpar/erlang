-module(customer).

-behaviour(gen_fsm).

%% API
-export([start/0]).

%% Callback functions
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

%% FSM states

-record(state, {waited, served}).

%%====================================================================
%% API
%%====================================================================

start() ->
    gen_fsm:start(?MODULE, [], []).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================

init([]) ->
    error_logger:info_msg("Customer: Good morning. My name is ~p~n", [self()]),
    {ok, wait, #state{waited = calendar:universal_time()}}.

handle_info(_Info, _StateName, StateData) ->
    {stop, undefined, StateData}.

handle_event(_Event, _StateName, StateData) ->
    {stop, undefined, StateData}.

handle_sync_event(_Event, _From, _StateName, StateData) ->
    {stop, undefined, none, StateData}.

terminate(_Reason, _StateName, _StateData) -> ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
    {ok, StateName, StateData}.

%%====================================================================
%% States and transitions
%%====================================================================

