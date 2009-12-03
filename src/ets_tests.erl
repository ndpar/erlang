-module(ets_tests).
-include_lib("eunit/include/eunit.hrl").

setup() ->
    ets:new(countries, [bag,named_table]),
    ets:insert(countries, {yves,france,cook}),
    ets:insert(countries, {sean,ireland,bartender}),
    ets:insert(countries, {marco,italy,cook}),
    ets:insert(countries, {chris,ireland,tester}).

teardown(_) ->
    ets:delete(countries).

match_test_() ->
    {foreach,
        fun setup/0,
        fun teardown/1,
        [
            fun no_match_after_elements_are_deleted/0,
            fun match_first_element_with_index_2/0,
            fun match_elements_but_dont_capture/0
        ]
    }.

no_match_after_elements_are_deleted() ->
    fun() ->
        ets:match_delete(countries, {'_',ireland,'_'}),
        ?assertEqual([], ets:match_object(countries, {'_',ireland,'_'}))
    end.

match_first_element_with_index_2() ->
    ?assertEqual([[sean],[chris]], ets:match(countries, {'$2',ireland,'_'})).

match_elements_but_dont_capture() ->
    ?assertEqual([[],[]], ets:match(countries, {'_',ireland,'_'})).
