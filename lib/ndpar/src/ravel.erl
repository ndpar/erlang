%%
%% @doc A problem of finding the shortest upravel.
%%
%% An <em>unravel</em> of a sequence `xs' is a bag of nonempty
%% subsequences of `xs' that when shuffled together can give back `xs'.
%% E.g. the letters of "accompany" can be unravelled into three lists:
%% "acm", "an" and "copy".
%%
%% An unravel is called an <em>upravel</em> if all its component sequences
%% are weakly increasing. Since each of "acm", "an" and "copy" is
%% increasing, they give an upravel of "accompany", and so do
%% "aany", "ccmp" and "o".
%% Each nonempty sequence has at least one upravel, namely the
%% upravel consisting of just singleton sequences. However, of all
%% possible upravels, we want to determine one with shortest size.
%%
%% @reference [B1] Chapter 8, pp. 50–55
%%
-module(ravel).
-author("Andrey Paramonov <github@ndpar.com>").

-export([supravel/1]).

-import(lists, [foldr/3]).


%%
%% @doc Finds shortest upravel of the given sequence.
%%
-spec supravel([A]) -> [[A]] when A :: term().
supravel(Sequence) -> foldr(fun insert/2, [], Sequence).


-spec insert(A, [[A]]) -> [[A]] when A :: term().
insert(X, []) -> [[X]];
insert(X, [[H | _] = Xs | Xss]) when X =< H -> [[X | Xs] | Xss];
insert(X, [Xs | Xss]) -> [Xs | insert(X, Xss)].


%% =============================================================================
%% Unit tests
%% =============================================================================

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

supravel_test_() ->
  [
    ?_assertEqual(["aany", "ccmp", "o"], supravel("accompany")),
    ?_assertEqual(
      [[1, 1, 2, 3, 5, 7], [3, 4, 5, 5, 8, 9], [6], [9]],
      supravel([3, 1, 4, 1, 5, 9, 2, 6, 5, 3, 5, 8, 9, 7]))
  ].

-endif.
