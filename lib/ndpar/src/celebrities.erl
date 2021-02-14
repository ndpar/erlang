%%
%% @doc A problem of finding celebrities.
%%
%% Imaging a set `P' of people at a party.
%% Say a subset `C' of `P' forms a <em>celebrity clique</em> if
%% `C' is nonempty, everybody at the party knows every member
%% of `C', but members of `C' know only each other.
%% Find this celebrity clique, assuming it exists.
%% Data for the problem:
%% 1) binary predicate `knows' and
%% 2) the set `P' as a list without duplicates.
%%
%% Note: the problem is not to determine whether or not such a clique exists.
%%
%% What is interesting about this problem is that it is asymptotically more
%% efficient to find a solution assuming one exists than to check that
%% it actually is a solution.
%%
%% @reference [B1] Chapter 9, pp. 56–63
%%
-module(celebrities).
-author("Andrey Paramonov <github@ndpar.com>").

-export([cclique/2]).

-import(lists, [foldr/3]).

%%
%% @doc Finds celebrity clique in linear time
%% <em>assuming it exists</em>.
%% If the clique does not exist, returns meaningless list.
%%
-spec cclique(Knows, [A]) -> [A] when
  Knows :: fun((A, A) -> boolean()),
  A :: term().

cclique(Knows, Ps) ->
  Op = fun(P, Cs) -> op(Knows, P, Cs) end,
  foldr(Op, [], Ps).

op(_, P, []) -> [P];
op(Knows, P, [C | _] = Cs) ->
  case {Knows(P, C), Knows(C, P)} of
    {false, _} -> [P];
    {_, false} -> Cs;
    _ -> [P | Cs]
  end.

%% =============================================================================
%% Unit tests
%% =============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

knows(_, {c, _}) -> true;
knows({c, _}, {p, _}) -> false;
knows({p, M}, {p, N}) when M < N -> true;
knows({p, _}, {p, _}) -> false.

clique_exists_test_() ->
  [
    ?_assertEqual(
      [{c, 1}, {c, 4}, {c, 3}],
      cclique(fun knows/2, [{c, 1}, {p, 6}, {c, 4}, {p, 2}, {p, 5}, {c, 3}]))
  ].

clique_doesnt_exist_test_() ->
  [
    ?_assertEqual(
      [{p, 1}], % this is not a celebrity clique
      cclique(fun knows/2, [{p, 1}, {p, 1}]))
  ].

-endif.
