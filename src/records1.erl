%
% F.Cesarini & S.Thomson, Erlang Programming, p.168.
% Exercise 7-1: Extending Records
%
-module(records1).
-export([birthday/1, joe/0, showPerson/1]).

-record(person, {name, age = 0, phone, address}).

birthday(#person{age = Age} = P) ->
    P#person{age = Age + 1}.

joe() ->
    #person{name = "Joe", age = 21, phone = "999-9999"}.

showPerson(#person{age = Age, phone = Phone, name = Name, address = Address}) ->
    io:format("name: ~p, age: ~p, phone: ~p, address: ~p~n", [Name, Age, Phone, Address]).
