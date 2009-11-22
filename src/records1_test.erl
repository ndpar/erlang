%
% F.Cesarini & S.Thomson, Erlang Programming, p.168.
% Exercise 7-1: Extending Records
%
-module(records1_test).
-export([test1/0]).

% 1> records1_test:test1().
%
test1() ->
    Joe = records1:joe(),
    records1:showPerson(Joe).
