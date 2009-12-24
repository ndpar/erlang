%
% http://www.erlang.org/eeps/eep-0011.html
%
-module(regex_tests).
-include_lib("eunit/include/eunit.hrl").

-define(BookPunctuation, "(\\ |\\,|\\.|\\;|\\:|\\t|\\n|\\(|\\))+").
-define(Punctuation, "(\\s|,|\\.|;|:|\\(|\\))+").

space_is_not_escaped_test() ->
    ?assertEqual(["first", "second"], re:split("first second", " ", [{return, list}])).

comma_is_not_escaped_test() ->
    ?assertEqual(["first", "second"], re:split("first,second", ",", [{return, list}])).

dot_is_escaped_test() ->
    ?assertEqual(["first", "second"], re:split("first.second", "\\.", [{return, list}])).

semicolumn_is_not_escaped_test() ->
    ?assertEqual(["first", "second"], re:split("first;second", ";", [{return, list}])).

column_is_not_escaped_test() ->
    ?assertEqual(["first", "second"], re:split("first:second", ":", [{return, list}])).


whitespace_character_test() ->
    ?assertEqual(["first", "second", "third", "forth"], re:split("first second\tthird\nforth", "\\s", [{return, list}])).

%punctuation_test() ->
%    ?assertEqual(["a", "b", "c", "d", "e", "f", "g"], re:split("a b,c.d;e:f(g)", ?Punctuation, [{return, list}, trim])).

capturing_region_test() ->
    ?assertEqual({match, [{3,4}]}, re:run("ABCabcdABC", "(abcd)", [{capture, [1]}])).

capturing_text_single_test() ->
    ?assertEqual({match, ["abcd"]}, re:run("ABCabcdABC", "(abcd)", [{capture, [1], list}])).

capturing_text_multiple_test() ->
    ?assertEqual({match, ["ABC", "ABC"]}, re:run("ABCabcdABC", "(ABC)", [{capture, all, list}])).

% Deprecated

%book_punctuation_test() ->
%    ?assertEqual({ok, ["first", "second"]}, regexp:split("first,second", ?BookPunctuation)).

%my_punctuation_test() ->
%    ?assertEqual({ok, ["first", "second"]}, regexp:split("first,second", ?Punctuation)).
