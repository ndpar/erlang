%%
%% F.Cesarini & S.Thomson, Erlang Programming, p.334.
%% Exercise 15-3: Peer to peer
%%
%% 1> peer:start().
%%
%% 1> peer:connect("localhost").
%% ok
%% 2> peer:send("Hello server").
%% ok
%% Client received reply: "Reply: Hello server"
%%
-module(peer).
-export([start/0, connect/1, send/1]).

-define(TCP_OPTIONS, [binary, {packet, 4}, {active, true}, {reuseaddr, true}]).

% Server

start() ->
    {ok, Listen} = gen_tcp:listen(1234, ?TCP_OPTIONS),
    seq_loop(Listen).

seq_loop(Listen) ->
    {ok, Socket} = gen_tcp:accept(Listen),
    loop(Socket),
    seq_loop(Listen).

loop(Socket) ->
    receive
        {tcp, Socket, Bin} ->
            Request = binary_to_term(Bin),
            io:format("Server received request: ~p~n", [Request]),
            Reply = "Reply: " ++ Request,
            gen_tcp:send(Socket, term_to_binary(Reply)),
            loop(Socket);
        {tcp_closed, Socket} ->
            io:format("Server socket closed~n")
    end.

% Client

connect(IpAddress) ->
    register(?MODULE, spawn_link(fun() -> connect_wait(IpAddress) end)),
    ok.

connect_wait(IpAddress) ->
    {ok, Socket} = gen_tcp:connect(IpAddress, 1234, [binary, {packet, 4}]),
    wait(Socket).

wait(Socket) ->
    receive
        {request, Request} ->
            ok = gen_tcp:send(Socket, term_to_binary(Request)),
            wait(Socket);
        {tcp, Socket, Bin} ->
            Reply = binary_to_term(Bin),
            io:format("Client received reply: ~p~n", [Reply]),
            wait(Socket);
        {tcp_closed, Socket} ->
            io:format("Client socket closed~n")
    end.

send(Request) ->
    ?MODULE ! {request, Request},
    ok.
