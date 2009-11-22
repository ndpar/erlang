%
% F.Cesarini & S.Thomson, Erlang Programming, p.169.
% Exercise 7-3: Database of Records
%
-module(db3).
-export([new/0, write/2, read/2, match/2, delete/2, destroy/1]).
-include("Data.hrl").

write(Db, Data) ->
    #data{key = Key, value = Element} = Data,
    lists:keystore(Key, 1, Db, {Key, Element}).

read(Db, Data) ->
    #data{key = Key} = Data,
    Entry = lists:keyfind(Key, 1, Db),
    case Entry of
        false -> {error, instance};
        {Key, Element} -> Data#data{value = Element}
    end.

match(Db, Data) ->
    #data{value = Element} = Data,
    [#data{key = Key, value = E} || {Key, E} <- Db, E == Element].

delete(Db, Data) ->
    #data{key = Key} = Data,
    lists:keydelete(Key, 1, Db),
    ok.

destroy(_Db) -> ok.

new() -> [].
