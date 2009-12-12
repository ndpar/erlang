%%
%% F.Cesarini & S.Thomson, Erlang Programming, p.292.
%% Exercise 12-3: Database Application
%%
%% 1> code:add_path("mydb/ebin").
%% true
%% 2> application:start(mydb).
%% ok
%% 3> mydb:read(baz).
%% {error,instance}
%% 4> application:stop(mydb).
%% ok
%% 5> whereis(mydb_sup).
%% undefined
%%
-module(mydb_app).
-export([start/2, stop/1]).
-behaviour(application).

start(_Type, _StartArgs) ->
    mydb_sup:start().

stop(_State) ->
    ok.

%% 1> application:start(mydb).
%% ok
%% 2> application:which_applications().
%% [{mydb,"In-memory Database","1.0"},
%%  {stdlib,"ERTS  CXC 138 10","1.16.3"},
%%  {kernel,"ERTS  CXC 138 10","2.13.3"}]
%% 3> appmon:start().
%% {ok,<0.50.0>}
