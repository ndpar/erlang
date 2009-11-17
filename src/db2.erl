%% Author: Andrey Paramonov
%% Created: Nov 7, 2009
%% Description: Simple DB (See "Erlang Programming", p.83)
-module(db2).
-export([new/0, destroy/1, write/3, read/2, match/2, delete/2]).

%%
%% Returns the list of keys for which Element is stored in the database 
%% match(Element, Db) -> lists:map(fun({Key,_}) -> Key end, (lists:filter(fun(X) -> {_,E} = X, E == Element end, Db))).
%%
match(Element, Db) -> [Key || {Key, E} <- Db, E == Element].

%%
%% Reads Element with the specified Key from database Db 
%%
read(Key, Db) ->
    Entry = lists:keyfind(Key, 1, Db),
    case Entry of
        false -> {error, instance};
        {Key,Element} -> {ok, Element}
    end.

%%
%% Deletes Element with specified Key from datase Db
%%
delete(Key, Db) -> lists:keydelete(Key, 1, Db).

%%
%% Writes tuple {Key, Element} into database Db
%%
write(Key, Element, Db) -> lists:keystore(Key, 1, Db, {Key,Element}).

%%
%% Drops the database
%%
destroy(_Db) -> ok.

%%
%% Creates new database
%%
new() -> [].

% Db = db2:new().
% Db1 = db2:write(francesco, london, Db).
% Db2 = db2:write(lelle, stockholm, Db1).
% db2:read(francesco, Db2).
% Db3 = db2:write(joern, stockholm, Db2).
% db2:read(ola, Db3).
% db2:match(stockholm, Db3).
% Db4 = db2:delete(lelle, Db3).
% db2:match(stockholm, Db4).
