%
% F.Cesarini & S.Thomson, Erlang Programming, p.169.
% Exercise 7-3: Database of Records
%
-module(db3_server_test).
-export([test1/0]).
-include("Data.hrl").

% 1> db3_server_test:test1().
% write(FooBar): ok
% read(Baz): {error,instance}
% read(Foo): {data,foo,bar}
% match(Bar): [{data,foo,bar}]
% ok
%
test1() ->
    db3_server:start(),

    FooBar = #data{key = foo, value = bar},
    io:format("write(FooBar): ~p~n", [db3_server:write(FooBar)]),

    Baz = #data{key = baz},
    io:format("read(Baz): ~p~n", [db3_server:read(Baz)]),

    Foo = #data{key = foo},
    io:format("read(Foo): ~p~n", [db3_server:read(Foo)]),

    Bar = #data{value = bar},
    io:format("match(Bar): ~p~n", [db3_server:match(Bar)]),

    db3_server:stop().
