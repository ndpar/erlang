%
% F.Cesarini & S.Thomson, Erlang Programming, p.170.
% Exercise 7-6: Parametrized Macros
%
% First compile without 'show' option
% $ erlc show_eval_test.erl
% then
% $ erlc -Dshow show_eval_test.erl
%
-module(show_eval_test).
-export([test/0]).
-include("Shapes.hrl").
-include("Debug.hrl").

test() ->
    test_show_eval(),
    ok.

test_show_eval() ->
    Area = ?SHOW_EVAL(shapes:area(#circle{radius = 10})),
    io:format("Printining from test: ~p~n", [Area]).
