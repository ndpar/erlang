-module(codemaker).

%% API
-export([start/0, guess/1]).
-export([loop/1]).

%%%===================================================================
%%% API
%%%===================================================================

%% @doc Generates random code and starts listening for guesses.
%% Registers a new process which dies after the correct guess.
-spec start() -> true.
start() ->
    register(?MODULE, spawn(?MODULE, loop, [position_list(random_row())])).

random_row() ->
    [r(), r(), r(), r()].

r() ->
    random:uniform(6).

position_list(Row) ->
    dict:to_list(positions(Row)).

positions(List) ->
    positions(List, 1, dict:new()).
positions([X|Xs], Pos, Acc) ->
    positions(Xs, Pos + 1, dict:append(X, Pos, Acc));
positions([], _, Acc) ->
    dict:map(fun(_, V) -> sets:from_list(V) end, Acc).


%% @doc Internal function: Starts a listner loop.
%% Responds on messages in the form of [number()].
-spec loop([{number(), [number()]}]) -> {Black::number(), White::number()} | congrats.
loop(Code) ->
    receive
        {Pid, Guess} ->
            case score(Code, Guess) of
                {4, 0} ->
                    Pid ! congrats;
                Score ->
                    Pid ! Score,
                    loop(Code)
            end
    end.

score(Code, Guess) ->
    score(Code, positions(Guess), {0, 0}).

score([{C,CPs}|Xs], Guess, {B, W}) ->
    case dict:find(C, Guess) of
        {ok, GPs} ->
            {NewB, NewW} = match(CPs, GPs),
            score(Xs, Guess, {B + NewB, W + NewW});
        _ ->
            score(Xs, Guess, {B, W})
    end;
score([], _, Acc) ->
    Acc.

match(Set1, Set2) ->
    B = sets:size(sets:intersection(Set1, Set2)),
    W = min(sets:size(Set1) - B, sets:size(Set2) - B),
    {B, W}.


%% @doc Submits a guess pattern to the codemaker.
%% Returns a tuple of black and white pegs.
-spec guess([number()]) -> {Black::number(), White::number()} | congrats.
guess(Pattern) ->
    ?MODULE ! {self(), Pattern},
    receive
        Feedback -> Feedback
    end.

%%%===================================================================
%%% Test Functions
%%%===================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

sc(Code, Guess, Result) ->
    ?assertEqual(Result, score(position_list(Code), Guess)).

score_test() ->
    sc([1,2,3,4], [5,5,2,2], {0,1}),
    sc([1,2,3,4], [4,3,3,5], {1,1}),
    sc([1,2,3,4], [1,4,1,5], {1,1}),
    sc([1,2,3,4], [6,6,4,5], {0,1}),
    sc([1,2,3,4], [2,3,1,4], {1,3}),
    sc([1,2,3,4], [1,3,2,4], {2,2}),
    sc([1,2,3,4], [1,2,3,4], {4,0}).

-endif.
