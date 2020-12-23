%%
%% @doc Various functions to work with modules.
%%
%% @reference [A2] Chapter 8 exercises
%%
-module(modules).
-export([all_loaded/0, is_loaded/1, exported/1]).

%%
%% @doc Returns a list of all modules that have been loaded in ERTS.
%%
-spec all_loaded() -> [module()].
all_loaded() -> [M || {M, _} <- code:all_loaded()].

%%
%% @doc Returns `true' if the Module has been loaded.
%%
-spec is_loaded(module()) -> boolean().
is_loaded(Module) -> lists:member(Module, all_loaded()).

%%
%% @doc Returns a list of functions exported by the Module.
%%
-spec exported(module()) -> [{atom(), non_neg_integer()}].
exported(Module) -> Module:module_info(exports).


-include_lib("eunit/include/eunit.hrl").

is_loaded_test() ->
  ?assert(is_loaded(?MODULE)).

exported_test() ->
  ?assert(lists:member({test, 0}, exported(?MODULE))).

% The following assertions have been tested in Erlang 23.1.5

% Exercise 1, p.138
dict_test() ->
  ?assertEqual(23, length(exported(dict))).

% Exercise 2, p.139
module_with_most_functions_test() ->
  ?assertMatch({erlang, _}, % 339
    lists:last(lists:keysort(2, [{M, length(exported(M))} || M <- all_loaded()]))).

% Exercise 2, p.139
most_common_function_name_test() ->
  AllFunctions = [F || M <- all_loaded(), {F, _} <- exported(M)],
  Functions = lists:keysort(2, maps:to_list(core:frequencies(AllFunctions))),
  ?assertMatch({module_info, _}, lists:last(Functions)). % 284

% Exercise 2, p.139
all_unambiguous_functions_test() ->
  AllFunctions = lists:usort([{F, M} || M <- all_loaded(), {F, _} <- exported(M)]),
  GroupFunctions = core:group_by(fun({F, _}) -> F end, AllFunctions),
  SingleModuleFuns = lists:filter(fun({_, L}) -> length(L) =:= 1 end, maps:to_list(GroupFunctions)),
  Result = lists:flatmap(fun({_, L}) -> L end, SingleModuleFuns),
  %io:format("~p~n", [Result]), % 1447 functions
  ?assert(lists:member({'*', erlang}, Result)).
