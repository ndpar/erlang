%%%
%%% Asynchronous Game of Life
%%% http://en.wikipedia.org/wiki/Conway's_Game_of_Life
%%%
%%% This implementation preserves the standard game behaviour by
%%% introducing a master process which represents a global clock.
%%%
%%% Another feature of this implementation is that new processes
%%% are spawned for live cells only, therefore it requires less
%%% processes than fully async implementation.
%%%
%%% See also life.erl for standard game implementation.
%%%
-module(life_async).

%% Published API
-export([start/2]).

%% Internal functions for process spawning
-export([new_cell/3]).

%% ------------------------------------------------------------------
%% Game Logic
%% ------------------------------------------------------------------

%% @doc Starts the new game and runs Iter iterations.
%% Seed is a list of unique cell coordinates.
%%
%% Blinker example: life_async:start([{3,2},{3,3},{3,4}], 2).
%%
%% Returns final generation.
start(Seed, Iter) ->
    god(Seed, new_world(), Iter).

%% ------------------------------------------------------------------
%% Master process
%% ------------------------------------------------------------------

%% @doc Main loop (usually runs in shell process).
%% Map is a list of coordinates where new cells to be born.
%% World is a hashtable of live cells. Keys are coordinates,
%% values are processes. One process per live cell.
god(Map, World, 0) ->
    ok = apocalypse(cells(World)),
    Map ++ coordinates(World);
god(Map, World, N) ->
    io:format("~p: ~p~n", [N, Map ++ coordinates(World)]),
    NewWorld = create(Map, World),
    ok = leave_them_alone(Map),
    NewMap = schedule_new_cells(NewWorld),
    god(NewMap, step(NewWorld), N-1).

%% @doc When game is over, all cells die.
apocalypse([]) -> ok;
apocalypse([Cell|Cells]) ->
    exit(Cell, kill),
    apocalypse(Cells).

%% @doc Make every new cell alive and add it to the World.
create([], World) -> World;
create([Coord|Cs], World) ->
    Cell = spawn(?MODULE, new_cell, [Coord, World, self()]),
    create(Cs, add(Coord, Cell, World)).

%% @doc Wait for every cell to confirm it is doing ok.
leave_them_alone([]) -> ok;
leave_them_alone([C|Cs]) ->
    receive
        {created, C} -> leave_them_alone(Cs)
    end.

%% @doc It is God's responsibility to decide which cell becomes
%% alive on the next step.
schedule_new_cells(World) ->
    Cells = cells(World),
    ok = ask_for_dead_neighbours(Cells),
    Counters = collect_dead_neighbours_info(Cells),
    new_map(Counters).

%% @doc Ask every cell to check which of its neighbours is dead.
ask_for_dead_neighbours([]) -> ok;
ask_for_dead_neighbours([Cell|Cells]) ->
    Cell ! {dead_neighbours, self()},
    ask_for_dead_neighbours(Cells).

%% @doc Wait for all cells to report about their dead neighbours.
%% Return a dictionary of frequencies, i.e. keys are coordinates
%% of dead cells, values are numbers of reports.
collect_dead_neighbours_info(Cells) ->
    lists:foldl(
        fun(Cell, Acc) ->
            receive
                {dead_cells, Cell, DeadCells} ->
                    update_counters(DeadCells, Acc)
            end
        end,
        dict:new(), Cells).

update_counters([], Counters) -> Counters;
update_counters([C|Cs], Counters) ->
    update_counters(Cs, dict:update_counter(C, 1, Counters)).

%% @doc Select which cells will be alive on the next step.
%% If 3 live cells reported about the same dead cell,
%% it will become alive.
new_map(Counters) -> new_map(dict:to_list(Counters), []).
new_map([], Acc) -> Acc;
new_map([{C,3}|Cs], Acc) -> new_map(Cs, [C|Acc]);
new_map([_|Cs], Acc) -> new_map(Cs, Acc).

%% @doc Runs one step of the game.
%% Let the live cells to choose their future, then collect survivals.
step(World) ->
    Cells = cells(World),
    ok = time_to_choose(Cells),
    filter_survivals(Cells, World).

time_to_choose([]) -> ok;
time_to_choose([Cell|Cells]) ->
    Cell ! {live_or_die, self()},
    time_to_choose(Cells).

filter_survivals(Cells, World) ->
    lists:foldl(
        fun(Cell, Acc) ->
            receive
                {dying, Cell, Coord} -> remove(Coord, Acc);
                {survived, Cell, _} -> Acc
            end
        end,
        World, Cells).

%% ------------------------------------------------------------------
%% Cell process
%% ------------------------------------------------------------------

%% @doc Initial steps of newly born cell.
new_cell(Coord, World, God) ->
    Cells = cells(World),
    ok = hello_world(Cells, Coord),
    Neighbours = know_your_neighbours(Cells, Coord),
    ok = mature(God, Coord),
    cell(Coord, Neighbours).

%% @doc Tell everyone about your existence.
hello_world([], _) -> ok;
hello_world([Cell|Cells], Coord) ->
    Cell ! {ping, self(), Coord},
    hello_world(Cells, Coord).

%% @doc Wait for everyone to respond to see who is your neighbour.
know_your_neighbours(Cells, Coord) ->
    lists:foldl(
        fun(Cell, Acc) ->
            receive
                {pong, Cell, NCoord} ->
                    case is_neighbour(Coord, NCoord) of
                        true -> add(NCoord, Cell, Acc);
                           _ -> Acc
                    end
            end
        end,
        new_world(), Cells).

is_neighbour({X1,Y1}, {X2,Y2}) ->
    Dx = abs(X1 - X2),
    Dy = abs(Y1 - Y2),
    Dx < 2 andalso Dy < 2.

%% @doc Master process is waiting for your maturity.
%% Send a message to unblock it.
mature(God, Coord) ->
    God ! {created, Coord},
    ok.

%% @doc Main loop of live cell is to respond to messages
%% from other cells and from master process.
cell(Coord, Neighbours) ->
    Cells = cells(Neighbours),
    receive
        {ping, Cell, NCoord} ->
            % Tell about yourself
            Cell ! {pong, self(), Coord},
            % and check if newcomer is your new neighbour
            case is_neighbour(Coord, NCoord) of
                true -> cell(Coord, add(NCoord, Cell, Neighbours));
                   _ -> cell(Coord, Neighbours)
            end;
        {live_or_die, God} ->
            % To be or not to be - it is actually your choice
            NewState = case population(Neighbours) of
                2 -> survived;
                3 -> survived;
                _ -> dying
            end,
            % Tell your neighbours about your choice
            lists:foreach(fun(N) -> N ! {NewState, self(), Coord} end, Cells),
            % Check which of your neighbours is going to die
            NewNeighbours = lists:foldl(
                fun(Cell, Acc) ->
                    receive
                        {dying, _, _} -> Acc;
                        {survived, Cell, NCoord} -> add(NCoord, Cell, Acc)
                    end
                end,
                new_world(), Cells),
            % Notify master process about your choice
            God ! {NewState, self(), Coord},
            case NewState of
                survived -> cell(Coord, NewNeighbours);
                dying -> ok
            end;
        {dead_neighbours, God} ->
            % help master process to conduct a census
            God ! {dead_cells, self(), dead_cells(Coord, Neighbours)},
            cell(Coord, Neighbours)
    end.

dead_cells(Coord, Neighbours) ->
    [N || N <- neighbours(Coord), not(lists:member(N, coordinates(Neighbours)))].

neighbours({X, Y}) ->
    [{X + DX, Y + DY} || DX <- [-1, 0, 1], DY <- [-1, 0, 1], {DX, DY} =/= {0, 0}].

%% ------------------------------------------------------------------
%% Data Abstractions
%%
%% Currently gb_trees are used. With Erlang 17 we can use frames.
%% ------------------------------------------------------------------

new_world() -> gb_trees:empty().

population(World) -> gb_trees:size(World).

add(Coord, Cell, World) -> gb_trees:insert(Coord, Cell, World).

remove(Coord, World) -> gb_trees:delete(Coord, World).

coordinates(World) -> gb_trees:keys(World).

cells(World) -> gb_trees:values(World).

