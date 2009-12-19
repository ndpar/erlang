%
% http://20bits.com/articles/erlang-a-generalized-tcp-server
%
% 1> echo_passive:start(1234).
%
% telnet localhost 1234
%
-module(echo_passive).
-author('Jesse E.I. Farmer <jesse@20bits.com>').
-export([start/1, listen/1]).

-define(TCP_OPTIONS, [{packet, 0}, {active, false}, {reuseaddr, true}]).

start(Port) ->
    spawn(?MODULE, listen, [Port]).

listen(Port) ->
    {ok, LSocket} = gen_tcp:listen(Port, ?TCP_OPTIONS),
    accept(LSocket).

% Wait for incoming connections and spawn the echo loop when we get one.
accept(LSocket) ->
    {ok, Socket} = gen_tcp:accept(LSocket),
    spawn(fun() -> loop(Socket) end),
    io:format("Client connected~n"),
    accept(LSocket).

% Echo back whatever data we receive on Socket.
loop(Socket) ->
    case gen_tcp:recv(Socket, 0) of
        {ok, Data} ->
            gen_tcp:send(Socket, Data),
            loop(Socket);
        {error, closed} ->
            io:format("Client disconnected~n")
    end.
