%
% Erlang implementation of some functions from clojure.core.
%
-module(core).
-export([frequencies/1]).

%
% Returns a map from distinct items in List to the number of times they appear.
%
frequencies(List) ->
  lists:foldl(fun update_count/2, #{}, List).

update_count(X, Map) ->
  maps:update_with(X, fun(C) -> C + 1 end, 1, Map).


-include_lib("eunit/include/eunit.hrl").

frequencies_test() ->
  ?assertEqual(#{1=>2, 2=>2, 3=>3, 4=>1},
    frequencies([1, 2, 3, 2, 3, 4, 1, 3])).
