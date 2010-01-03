%%
%% @author Andrey Paramonov <erlang@ndpar.com>
%% @doc Application module for Distributed Cache
%% @copyright 2009 Andrey Paramonov
%%
-module(mycache_app).
-export([start/2, stop/1]).
-behaviour(application).

%%
%% @doc Starts application
%% @spec start(Type, StartArgs) -> Result
%% Result = {ok,Pid} | ignore | {error,Error}
%% Pid = pid()
%% Error = {already_started,Pid} | shutdown | term()
%%
start(_Type, _StartArgs) ->
    mycache_sup:start().

%%
%% @doc Stops application
%% @spec stop(State) -> ok
%%
stop(_State) ->
    ok.
