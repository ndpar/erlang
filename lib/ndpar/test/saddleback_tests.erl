%%
%% @doc Performance tests of the `saddleback' search algorithm.
%%
%% @reference [B1] pp. 19–20
%% ```
%% > fprof:apply(saddleback, invert4, [fun(X, Y) -> 3 * X + 27 * Y + Y * Y end, 5000]).
%% > fprof:profile().
%% > fprof:analyse([{dest, "invert42.txt"}]).
%%
%% > fprof:apply(saddleback, invert3, [fun(X, Y) -> 3 * X + 27 * Y + Y * Y end, 5000]).
%% > fprof:profile().
%% > fprof:analyse([{dest, "invert32.txt"}]).
%% '''
-module(saddleback_tests).
-author("Andrey Paramonov <github@ndpar.com>").

f0(X, Y) -> maths:pow(2, Y) * (2 * X + 1) - 1.

f1(X, Y) -> X * maths:pow(2, X) + Y * maths:pow(2, Y) + 2 * X + Y.

f2(X, Y) -> 3 * X + 27 * Y + Y * Y.

f3(X, Y) -> X * X + Y * Y + X + Y.

f4(X, Y) -> X + maths:pow(2, Y) + Y + 1.

f5(X, Y) -> X + Y.

-include_lib("eunit/include/eunit.hrl").

ifuns() -> [
  fun saddleback:invert2/2,
  fun saddleback:invert3/2,
  fun saddleback:invert4/2
].

%%
%% Count the number of invocations of function `f'
%% under different versions of the inversion algorithm.
%%
count_test_() ->
  cprof:start(),

  F0 = [prof(IFun, fun f0/2, f0, 5000) || IFun <- ifuns()],
  F1 = [prof(IFun, fun f1/2, f1, 5000) || IFun <- ifuns()],
  F2 = [prof(IFun, fun f2/2, f2, 5000) || IFun <- ifuns()],
  F3 = [prof(IFun, fun f3/2, f3, 5000) || IFun <- ifuns()],
  F4 = [prof(IFun, fun f4/2, f4, 5000) || IFun <- ifuns()],

  cprof:stop(),
  [ %              inv2  inv3 inv4
    ?_assertEqual([7501, 2538, 129], F0), %  2ʸ(2x + 1) - 1
    ?_assertEqual([5011,   44,  46], F1), %  x 2ˣ + y 2ʸ + 2x + y
    ?_assertEqual([6668, 1751, 464], F2), %  3x + 27y + y²
    ?_assertEqual([5068,  164, 204], F3), %  x² + y² + x + y
    ?_assertEqual([9987, 5024, 143], F4)  %  x + 2ʸ + y + 1
  ].

prof(IFun, Fun, F, Z) ->
  cprof:restart(),
  IFun(Fun, Z),
  fun_count_from(cprof:analyse(?MODULE), F).

fun_count_from({?MODULE, _, FunCalls}, Fun) -> fun_count(FunCalls, Fun).

fun_count([], _) -> 0;
fun_count([{{_, Fun, _}, N} | _], Fun) -> N;
fun_count([_ | Cs], Fun) -> fun_count(Cs, Fun).


f5_test_() ->
  cprof:start(),
  F5 = [prof(IFun, fun f5/2, f5, 5) || IFun <- [fun saddleback:invert1/2 | ifuns()]],
  cprof:stop(),
  [?_assertEqual([36, 6, 12, 22], F5)].
