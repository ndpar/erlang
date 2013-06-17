-module(barber).
-author("Andrey Paramonov <github@ndpar.com>").
-behaviour(gen_fsm).

%% API
-export([start_link/0, new_customer/1]).

%% Callback functions
-export([init/1, handle_event/3, handle_sync_event/4, handle_info/3,
         terminate/3, code_change/4]).

%% FSM states
-export([busy/2, sleep/2]).

-define(SERVER, ?MODULE).
-define(HAIRCUT_TIME, 5000).
-define(ROOM_SIZE, 3).

-record(state, {chair, room = []}).

%%====================================================================
%% API
%%====================================================================

start_link() ->
    gen_fsm:start_link({local, ?SERVER}, ?MODULE, [], []).

new_customer(Customer) ->
    gen_fsm:send_event(?SERVER, {new, Customer}).

%%====================================================================
%% gen_fsm callbacks
%%====================================================================

init([]) ->
    log("Shop is open. zzzzZ"),
    {ok, sleep, #state{}}.

handle_info(finish, busy, #state{chair = Customer, room = Room}) ->
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
    end.

serving(Customer) ->
    customer:sit_down(Customer),
    timer:send_after(?HAIRCUT_TIME, finish).

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

sleep({new, Customer}, StateData) ->
    log("Good morning ~p. Please take a seat.", [Customer]),
    serving(Customer),
    NewStateData = StateData#state{chair = Customer},
    {next_state, busy, NewStateData}.

busy({new, Customer}, #state{room = Room} = StateData)
  when length(Room) == ?ROOM_SIZE ->
    log("Sorry ~p. Not today.", [Customer]),
    customer:sorry(Customer),
    {next_state, busy, StateData};

busy({new, Customer}, #state{room = Room} = StateData) ->
    log("I'm busy. You have to wait."),
    customer:wait(Customer),
    NewStateData = StateData#state{room = Room ++ [Customer]},
    {next_state, busy, NewStateData}.

%%====================================================================
%% Helper functions
%%====================================================================

log(Message) ->
    log(Message, []).

log(Message, Args) ->
    error_logger:info_msg("~p: " ++ Message ++ "~n", [self() | Args]).
