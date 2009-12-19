%%
%% F.Cesarini & S.Thomson, Erlang Programming, p.334.
%% Exercise 15-1: Snooping HTTP request
%%
%% Based on echo.erl
%%
-module(http_snoop).
-export([start/1, listen/1]).

-define(TCP_OPTIONS, [{packet, 0}, {active, false}, {reuseaddr, true}]).

start(Port) ->
    spawn(?MODULE, listen, [Port]).

listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket).

accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> loop(Socket) end),
    accept(LSocket).

loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            io:format("Data: ~p~n", [Data]),
            gen_tcp:send(Socket, Data),
            gen_tcp:close(Socket),
            loop(Socket);
        {error, closed} ->
            ok
    end.
