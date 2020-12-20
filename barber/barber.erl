-module(barber).
-author("Andrey Paramonov <github@ndpar.com>").
-behaviour(gen_statem).

%% API
-export([open_shop/0, new_customer/1]).

%% Callback functions
-export([init/1, callback_mode/0, terminate/3, code_change/4]).

%% FSM states
-export([sleep/3, ready/3, busy/3]).

-define(SERVER, ?MODULE).
-define(HAIRCUT_TIME, 5000).
-define(ROOM_SIZE, 3).

-record(state, {room = 0}).

%%====================================================================
%% API
%%====================================================================

open_shop() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec new_customer(pid()) -> ok.
new_customer(Customer) ->
  gen_statem:cast(?SERVER, {new, Customer}).

%%====================================================================
%% gen_statem callbacks
%%====================================================================

callback_mode() ->
  state_functions.

init([]) ->
  log("Shop is open. zzzzZ"),
  {ok, sleep, #state{}}.

terminate(_Reason, _StateName, _StateData) ->
  ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

%%====================================================================
%% FSM states and transitions
%%====================================================================

sleep(cast, {new, Customer}, #state{room = 0} = StateData) ->
  log("Waking up for ~p.", [Customer]),
  {next_state, ready, StateData, {next_event, cast, {new, Customer}}}.

ready(cast, {new, Customer}, StateData) ->
  log("Good morning ~p. Please take a seat.", [Customer]),
  customer:sit_down(Customer),
  {next_state, busy, StateData, {state_timeout, ?HAIRCUT_TIME, Customer}}.

busy(state_timeout, Customer, #state{room = Room}) ->
  log("Do you like your haircut ~p?", [Customer]),
  ok = customer:done(Customer),
  case Room of
    0 ->
      log("Time for nap. zzzzZ"),
      {next_state, sleep, #state{}};
    _ ->
      log("Next please.", []),
      {next_state, ready, #state{}} % postponed events will re-increment room on re-play
  end;

busy(cast, {new, Customer}, #state{room = ?ROOM_SIZE} = StateData) ->
  log("Sorry ~p. No room left.", [Customer]),
  customer:sorry(Customer),
  {next_state, busy, StateData};

busy(cast, {new, _Customer}, #state{room = Room}) ->
  {next_state, busy, #state{room = Room + 1}, postpone}.

%%====================================================================
%% Helper functions
%%====================================================================

log(Message) ->
  log(Message, []).

log(Message, Args) ->
  error_logger:info_msg("~p: " ++ Message ++ "~n", [self() | Args]).
