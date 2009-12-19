%%
%% F.Cesarini & S.Thomson, Erlang Programming, p.334.
%% Exercise 15-2: HTTP proxy
%%
%% Based on http_snoop.erl
%%
-module(http_proxy).
-export([start/1, listen/1]).

-define(TCP_OPTIONS, [{packet, 0}, {active, false}, {reuseaddr, true}]).

start(Port) ->
    inets:start(),
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
            io:format("~p~n", [Data]),
            gen_tcp:send(Socket, get_response(get_request_url(Data))),
            gen_tcp:close(Socket),
            loop(Socket);
        {error, closed} ->
            ok
    end.

get_request_url(Request) ->
    {match, [Url]} = re:run(Request, "GET (.+?) HTTP", [{capture, [1], list}]),
    Url.

get_response(RequestUrl) ->
    {ok, {_Http, _Headers, Body}} = http:request(RequestUrl),
    Body.
