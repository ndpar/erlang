%
% F.Cesarini & S.Thomson, Erlang Programming, p.84.
% Exercise 3-5: Manipulating lists
% Exercise 3-6: Sorting lists
%
-module(recursion).
-export([average/1,
         bump/1,
         concatenate/1,
         create/1, create/2,
         create_r/1, create_r/2,
         filter/2,
         flatten/1,
         merge/2,
         msort/1,
         print/1,
         qsort/1,
         reverse/1,
         split/1,
         sum/1, sum/2]).

bump(List) -> bump_acc(List, []).
bump_acc([], Acc) -> Acc;
bump_acc([H|T], Acc) -> bump_acc(T, Acc ++ [H + 1]).

average(List) -> average_acc(List, 0, 0).
average_acc([], _, 0) -> 0;
average_acc([], Sum, Len) -> Sum/Len;
average_acc([H|T], Sum, Len) -> average_acc(T, Sum + H, Len + 1).

sum(N) -> sum_acc(1, N, 0).
sum(N, M) -> sum_acc(N, M, 0).
sum_acc(N, M, Sum) when N < M -> sum_acc(N, M - 1, Sum + M);
sum_acc(N, N, Sum) -> Sum + N.

create(N) -> create(1, N).
create(N, M) -> create_acc(N, M, []).
create_acc(N, M, List) when N < M -> create_acc(N, M - 1, [M | List]);
create_acc(N, N, List) -> [N | List].

create_r(N) -> create_r(1, N).
create_r(N, M) -> create_r_acc(N, M, []).
create_r_acc(N, M, List) when N < M -> create_r_acc(N + 1, M, [N | List]);
create_r_acc(M, M, List) -> [M | List].

print(N) when 0 < N -> io:format("Number: ~p~n", [N]), print(N - 1);
print(N) when N < 0 -> io:format("Number: ~p~n", [N]), print(N + 1);
print(0) -> io:format("Number: ~p~n", [0]).

filter(List, N) -> filter_acc(List, N, []).
filter_acc([], _, Acc) -> Acc;
filter_acc([M | Tail], N, Acc) when M =< N -> filter_acc(Tail, N, Acc ++ [M]);
filter_acc([_ | Tail], N, Acc) -> filter_acc(Tail, N, Acc).

reverse([]) -> [];
reverse([X | Tail]) -> reverse(Tail) ++ [X].

concatenate([]) -> [];
concatenate([X | Tail]) -> X ++ concatenate(Tail).

flatten([]) -> [];
flatten([H | T]) -> flatten(H) ++ flatten(T);
flatten(X) -> [X].

split(List) -> split_acc(List, {[],[]}).
split_acc([], Acc) -> Acc;
split_acc([F,S | Tail], {First, Scnd}) -> split_acc(Tail, {First ++ [F], Scnd ++ [S]});
split_acc([H], {First, Scnd}) -> {First ++ [H], Scnd}.

merge(Xs, Ys) -> lists:reverse(mergeL(Xs,Ys,[])).
mergeL([X|Xs],Ys,Zs) -> mergeR(Xs,Ys,[X|Zs]);
mergeL([],[],Zs) -> Zs.
mergeR(Xs,[Y|Ys],Zs) -> mergeL(Xs,Ys,[Y|Zs]);
mergeR([],[],Zs) -> Zs.

qsort([]) -> [];
qsort([H | Tail]) -> qsort([Y || Y <- Tail, Y < H]) ++ [H] ++ qsort([Y || Y <- Tail, Y >= H]).

% http://en.wikipedia.org/wiki/Merge_sort
msort([]) -> [];
msort([X]) -> [X];
msort(List) ->
    {Left, Right} = split(List),
    merge_ordered(msort(Left), msort(Right)).

merge_ordered(Left, Right) -> merge_ordered_acc(Left, Right, []).
merge_ordered_acc([], [], Acc) -> Acc;
merge_ordered_acc(Left, [], Acc) -> Acc ++ Left;
merge_ordered_acc([], Right, Acc) -> Acc ++ Right;
merge_ordered_acc([X|Xs], [Y|Ys], Acc) when X < Y -> merge_ordered_acc(Xs, [Y|Ys], Acc ++ [X]);
merge_ordered_acc([X|Xs], [Y|Ys], Acc) -> merge_ordered_acc([X|Xs], Ys, Acc ++ [Y]).
