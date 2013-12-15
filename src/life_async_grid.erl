%%%
%%% Asynchronous Game of Life
%%% http://en.wikipedia.org/wiki/Conway's_Game_of_Life
%%% http://en.wikipedia.org/wiki/Asynchronous_cellular_automaton
%%%
%%% This implementation can be run step by step, controlled from
%%% the shell, in which case it preserves the classic game behaviour.
%%% Or it can be run completely asynchronously until it's stopped by
%%% `stop` message. In this case the behaviour of the game is
%%% eventually consistent, meaning that at any particular point
%%% there might be cells on the grid from different generations,
%%% which you wouldn't expect in the classic game. However,
%%% since cells "synchronize" with each other, the results of the
%%% async game "on average" will be the same as in the classic one.
%%%
%%% In either case, the default state of the grid can be examined
%%% by running `snapshot` command.
%%%
%%% To run the game forever, uncomment line 148.
%%%
%%% See also:
%%% life.erl - standard game implementation,
%%% life_async - async implementation with synchronization process.
%%%
-module(life_async_grid).
-author("Andrey Paramonov <github@ndpar.com>").

%% Published API
-export([new_game/1, make_alive/2, start/1, snapshot/1, stop/1]).

%% Internal functions for process spawning
-export([cell/1]).

%% Cell state as a convenient data holder
-record(cell, {coord,
               gen = 0,
               state = dead,
               prev_state = dead,
               response_count = 0,
               alive_count = 0,
               neighbours = []}).

%% ------------------------------------------------------------------
%% Game Logic
%%
%% Grid = life_async_grid:new_game(10).
%% life_async_grid:make_alive([{3,2},{3,3},{3,4}], Grid).
%% life_async_grid:snapshot(Grid).
%% life_async_grid:start(Grid).
%% life_async_grid:snapshot(Grid).
%% life_async_grid:stop(Grid).
%% ------------------------------------------------------------------

%% @doc Creates a new grid of connected dead cells.
%% Users should keep the reference to the grid to play the game.
new_game(Size) ->
    Grid = new_grid(Size),
    connect_all(Grid),
    Grid.

%% @doc Builds a new grid of disconnected dead cells.
%% Grid is a hashmap where keys are coordinates and values are
%% cell processes.
new_grid(Size) ->
    lists:foldl(
        fun(Coord, Acc) ->
                Cell = spawn(?MODULE, cell, [#cell{coord = Coord}]),
                add(Coord, Cell, Acc)
        end,
        new_grid(), cartesian_plane(Size)).

%% @doc Sends `connect` message to all cells.
%% It's done once during the initialization of the game.
%% Upon receiving this message, cells will discover their
%% neighbours and save reference to them.
connect_all(Grid) ->
    ok = send(Grid, coordinates(Grid), {connect, Grid}).

%% @doc Sends `alive` message to specific cells.
%% Upon receiving this message, cells will change their state to alive.
make_alive(Coords, Grid) ->
    ok = send(Grid, Coords, alive).

%% @doc Sends `start` message to all cells.
%% That will initiate the first step of the game.
start(Grid) ->
    ok = send(Grid, coordinates(Grid), start).

%% @doc Sends `current_status` message to all cells, requesting
%% their status. Blocks until receiving responses from all cells.
%% Returns coordinates of all live cells together with their
%% generation numbers.
snapshot(Grid) ->
    All = coordinates(Grid),
    ok = send(Grid, All, {current_status, self()}),
    lists:foldl(
        fun(C, Acc) ->
                receive
                    {current_status, C, alive, Gen} -> [{C,Gen}|Acc];
                    {current_status, C, dead, _} -> Acc
                end
        end, [], All).

%% @doc Sends `stop` message to all cells, forcing them to leave game.
stop(Grid) ->
    ok = send(Grid, coordinates(Grid), stop).

%% ------------------------------------------------------------------
%% Cell process
%% ------------------------------------------------------------------

%% @doc Main loop of a cell is to respond to messages from shell
%% and other cells.
cell(State) ->
    CurrentGen = State#cell.gen,
    PrevGen = CurrentGen - 1,
    receive
        {connect, Grid} ->
            NCoords = neighbours(State#cell.coord, grid_size(Grid)),
            NewNeighbours = [cell(C, Grid) || C <- NCoords],
            cell(State#cell{neighbours = NewNeighbours});
        alive ->
            cell(State#cell{state = alive});
        {current_status, Sender} ->
            Sender ! {current_status, State#cell.coord, State#cell.state, State#cell.gen},
            cell(State);
        {your_status, CurrentGen, Sender} ->
            Sender ! {my_status, State#cell.state},
            cell(State);
        {your_status, PrevGen, Sender} ->
            Sender ! {my_status, State#cell.prev_state},
            cell(State);
        start ->
            self() ! step,
            cell(State);
        step ->
            lists:foreach(
                fun(N) -> N ! {your_status, State#cell.gen, self()} end,
                State#cell.neighbours),
            cell(State);
        {my_status, NStatus} ->
            NewState = new_state(State, NStatus),
            self() ! {transition, NewState#cell.response_count, NewState#cell.alive_count},
            cell(NewState);
        {transition, 8, NAlive} ->
            NewStatus = new_status(State#cell.state, NAlive),
            %% uncomment to disable global clock
            %self() ! step,
            cell(State#cell{prev_state = State#cell.state, state = NewStatus,
                            gen = State#cell.gen + 1, response_count = 0, alive_count = 0});
        {transition, _, _} ->
            cell(State);
        stop -> ok
    end.

%% @doc Updates response count and live neighbours count.
new_state(State, alive) ->
    State#cell{response_count = State#cell.response_count + 1,
               alive_count = State#cell.alive_count + 1};
new_state(State, dead) ->
    State#cell{response_count = State#cell.response_count + 1}.

%% @doc Implementation of game rules.
new_status(_, 3) -> alive;
new_status(alive, 2) -> alive;
new_status(_, _) -> dead.

%% ------------------------------------------------------------------
%% Helper functions
%% ------------------------------------------------------------------

%% @doc Returns cartesian square plane with side length of Size.
cartesian_plane(Size) ->
    [{X,Y} || X <- seq(Size), Y <- seq(Size)].

seq(Size) -> lists:seq(0, Size - 1).

grid_size(Grid) -> round(math:sqrt(population(Grid))).

%% @doc The game grid is modelled as torus.
neighbours({X, Y}, Size) ->
    [{mod(X + DX, Size), mod(Y + DY, Size)} ||
        DX <- [-1, 0, 1],
        DY <- [-1, 0, 1],
        {DX, DY} =/= {0, 0}].

mod(X,Y) -> (X rem Y + Y) rem Y.

%% @doc Sends Message to cells with specified Coords on the Grid.
send(Grid, Coords, Message) ->
    lists:foreach(
        fun(Coord) ->
                cell(Coord, Grid) ! Message
        end,
        Coords),
    ok.

%% ------------------------------------------------------------------
%% Data Abstractions
%%
%% Currently dict is used. With Erlang 17 we can use frames.
%% ------------------------------------------------------------------

new_grid() -> dict:new().

population(Grid) -> dict:size(Grid).

add(Coord, Cell, Grid) -> dict:store(Coord, Cell, Grid).

coordinates(Grid) ->
    [Coord || {Coord, _} <- dict:to_list(Grid)].

cell(Coord, Grid) -> dict:fetch(Coord, Grid).

