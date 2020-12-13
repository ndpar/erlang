-module(customer).
-author("Andrey Paramonov <github@ndpar.com>").
-behaviour(gen_statem).

%% API
-export([start/0, sit_down/1, done/1, wait/1, sorry/1]).

%% Callback functions
-export([init/1, callback_mode/0, terminate/3, code_change/4]).

%% FSM states
-export([waiting/3, served/3]).

%%====================================================================
%% API
%%====================================================================

start() ->
  {ok, Pid} = gen_statem:start(?MODULE, [], []),
  Pid.

sit_down(Customer) ->
  gen_statem:cast(Customer, sit_down).

done(Customer) ->
  gen_statem:cast(Customer, exit).

wait(Customer) ->
  gen_statem:cast(Customer, wait).

sorry(Customer) ->
  gen_statem:cast(Customer, exit).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================

callback_mode() ->
  state_functions.

init([]) ->
  log("Good morning."),
  {ok, waiting, unow()}.

terminate(_Reason, _StateName, _StateData) ->
  ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

%%====================================================================
%% FSM states and transitions
%%====================================================================

waiting(cast, sit_down, WaitStart) ->
  log("I've been waiting for ~p sec.", [duration(WaitStart)]),
  {next_state, served, unow()};

waiting(cast, wait, StateData) ->
  log("OK."),
  {next_state, waiting, StateData};

waiting(cast, exit, _StateData) ->
  log("Maybe tomorrow."),
  {stop, normal, undefined}.

served(cast, exit, ServiceStart) ->
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
