%% Author: Andrey Paramonov
%% Created: Nov 7, 2009
%% Description: Simple DB (See "Erlang Programming", p.83)
-module(db).
-export([new/0, destroy/1, write/3, read/2, match/2, delete/2]).

%%
%% Returns the list of keys for which Element is stored in the database 
%%
match(_, []) -> [];
match(Element, [{Key, Element} | Tail]) -> [Key | match(Element, Tail)];
match(Element, [_ | Tail]) -> match(Element, Tail).

%%
%% Reads Element with the specified Key from database Db 
%%
read(_, []) -> {error, instance};
read(Key, [{Key, Element} | _]) -> {ok, Element};
read(Key, [_ | Tail]) -> read(Key, Tail).

%%
%% Deletes Element with specified Key from datase Db
%%
delete(_, []) -> [];
delete(Key, [{Key, _} | Tail]) -> Tail;
delete(Key, [Entry | Tail]) -> [Entry | delete(Key, Tail)].

%%
%% Writes tuple {Key, Element} into database Db
%%
write(Key, Element, Db) -> [{Key, Element} | delete(Key, Db)].

%%
%% Drops the database
%%
destroy(_Db) -> ok.

%%
%% Creates new database
%%
new() -> [].
