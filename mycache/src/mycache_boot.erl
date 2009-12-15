-module(mycache_boot).
-export([start/0]).

start() ->
    application:start(mycache).
