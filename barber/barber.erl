-module(barber).
-author("Andrey Paramonov <github@ndpar.com>").
-behaviour(gen_statem).

%% API
-export([open_shop/0, new_customer/1]).

%% Callback functions
-export([init/1, callback_mode/0, terminate/3, code_change/4]).

%% FSM states
-export([busy/3, sleep/3]).

-define(SERVER, ?MODULE).
-define(HAIRCUT_TIME, 5000).
-define(ROOM_SIZE, 3).

-record(state, {chair, room = []}).

%%====================================================================
%% API
%%====================================================================

open_shop() ->
  gen_statem:start_link({local, ?SERVER}, ?MODULE, [], []).

-spec new_customer(pid()) -> ok.
new_customer(Customer) ->
  gen_statem:cast(?SERVER, {new, Customer}).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================

callback_mode() ->
  state_functions.

init([]) ->
  log("Shop is open. zzzzZ"),
  {ok, sleep, #state{}}.

serving(Customer) ->
  customer:sit_down(Customer),
  timer:send_after(?HAIRCUT_TIME, finish).

terminate(_Reason, _StateName, _StateData) ->
  ok.

code_change(_OldVsn, StateName, StateData, _Extra) ->
  {ok, StateName, StateData}.

%%====================================================================
%% FSM states and transitions
%%====================================================================

sleep(cast, {new, Customer}, StateData) ->
  log("Good morning ~p. Please take a seat.", [Customer]),
  serving(Customer),
  {next_state, busy, StateData#state{chair = Customer}}.

busy(info, finish, #state{chair = Customer, room = Room}) ->
  log("Do you like your haircut ~p?", [Customer]),
  customer:done(Customer),
  case Room of
    [C | Rest] ->
      log("Next please ~p.", [C]),
      serving(C),
      NewStateData = #state{chair = C, room = Rest},
      {next_state, busy, NewStateData};
    [] ->
      log("Time for nap. zzzzZ."),
      {next_state, sleep, #state{}}
  end;

busy(cast, {new, Customer}, #state{room = Room} = StateData) when length(Room) == ?ROOM_SIZE ->
  log("Sorry ~p. Not today.", [Customer]),
  customer:sorry(Customer),
  {next_state, busy, StateData};

busy(cast, {new, Customer}, #state{room = Room} = StateData) ->
  log("I'm busy. You have to wait."),
  customer:wait(Customer),
  {next_state, busy, StateData#state{room = Room ++ [Customer]}}.

%%====================================================================
%% Helper functions
%%====================================================================

log(Message) ->
  log(Message, []).

log(Message, Args) ->
  error_logger:info_msg("~p: " ++ Message ++ "~n", [self() | Args]).
