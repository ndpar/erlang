-module(my_lists).
-export([qsort/1, keyfilter/3]).

qsort([])     -> [];
qsort([X|Xs]) -> qsort([Y || Y<-Xs, Y<X]) ++ [X] ++ qsort([Y || Y<-Xs, Y>=X]).

keyfilter(Key, N, TupleList) ->
    lists:filter(fun(Elem) -> tuple_match(Key, N, Elem) end, TupleList).

tuple_match(Key, N, Tuple) ->
    case lists:keyfind(Key, N, [Tuple]) of
        Tuple -> true;
        false -> false
    end.