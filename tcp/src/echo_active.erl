%
% http://forums.pragprog.com/forums/27/topics/390
%
% 1> echo_active:start(4321).
%
% telnet localhost 4321
%
-module(echo_active).
-author('Alain O\'Dea').
-export([start/1]).

-define(TCP_OPTIONS, [list, {packet, line}, {reuseaddr, true}, {active, true}]).

start(Port) ->
    {ok, Listen} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    spawn(fun() -> par_connect(Listen) end).

par_connect(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    spawn(fun() -> par_connect(Listen) end),
    io:format("Client connected~n"),
    echo(Socket). 

echo(Socket) ->
    receive
        {tcp, Socket, Line} ->
            gen_tcp:send(Socket, Line),
            echo(Socket);
        {tcp_closed, Socket} ->
            io:format("Client disconnected~n")
    end.
