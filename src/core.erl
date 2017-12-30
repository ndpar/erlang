%
% Erlang implementation of some functions from clojure.core.
%
-module(core).
-export([frequencies/1, group_by/2]).

%
% Returns a map from distinct items in List to the number of times they appear.
%
% See https://clojuredocs.org/clojure.core/frequencies
%
frequencies(List) ->
  lists:foldl(fun update_count/2, #{}, List).

update_count(X, Map) ->
  maps:update_with(X, fun(C) -> C + 1 end, 1, Map).

%
% Returns a map of the elements of List keyed by the result of
% Fun on each element. The value at each key will be a list of the
% corresponding elements, in the order they appeared in List.
%
% See https://clojuredocs.org/clojure.core/group-by
%
group_by(Fun, List) ->
  F = fun(E, Map) ->
    K = Fun(E),
    maps:update_with(K, fun(L) -> [E | L] end, [E], Map)
  end,
  maps:map(fun(_, V) -> lists:reverse(V) end, lists:foldl(F, #{}, List)).


-include_lib("eunit/include/eunit.hrl").

frequencies_test() ->
  ?assertEqual(#{1=>2, 2=>2, 3=>3, 4=>1},
    frequencies([1, 2, 3, 2, 3, 4, 1, 3])).

group_by_test() ->
  ?assertEqual(#{1=>["a"], 2=>["as", "aa"], 3=>["asd"], 4=>["asdf", "qwer"]},
    group_by(fun erlang:length/1, ["a", "as", "asd", "aa", "asdf", "qwer"])).
