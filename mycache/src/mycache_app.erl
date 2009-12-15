-module(mycache_app).
-export([start/2, stop/1]).
-behaviour(application).

start(_Type, _StartArgs) ->
    mycache_sup:start().

stop(_State) ->
    ok.
