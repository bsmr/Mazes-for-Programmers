%%%-------------------------------------------------------------------
%%% @author Boris Mühmer <boris.muehmer@gmail.com>
%%% @copyright (C) 2015, Boris Mühmer
%%% @doc
%%%
%%% @end
%%% Created : 18 Jul 2015 by Boris Mühmer <boris.muehmer@gmail.com>
%%%-------------------------------------------------------------------
-module(maze_example).

-export([start/0]).

start() ->
    ok = application:start('sasl'),
    ok = application:start('maze'),
    %%ok = observer:start(),
    example(),
    init:stop().
    %%ok.

example() ->
    {ok, Grid1} = maze:create(sidewinder, 4, 4),
    %%{ok, Grid1} = maze:create(sidewinder, 15, 50),
    ok = maze_grid:configure(Grid1),
    {ok, Cells1} = maze_grid:cells(Grid1),

    binary_tree(Cells1),
    
    %%{error, out_of_bounds} = maze_grid:at(Grid1, -1, -1),
    %%{error, out_of_bounds} = maze_grid:at(Grid1,  2,  4),
    %%{error, out_of_bounds} = maze_grid:at(Grid1,  4,  2),
    %%{error, out_of_bounds} = maze_grid:at(Grid1,  4,  4),
    %%{ok, Cell1} = maze_grid:at(Grid1, 2, 3),
    %%{ok, Text1} = maze_cell:to_string(Cell1),
    %%io:format("~s~n", [Text1]),
    {ok, Data1} = maze_grid:to_string(Grid1),
    io:format("~s", [Data1]),
    %%{ok, Grid2} = maze:create(foobar, 10, 10),
    %%{ok, Data2} = maze_grid:to_string(Grid2),
    %%io:format("~s~n", [Data2]),
    ok.

binary_tree(Cells) ->
    %%io:format("*** Cells: ~p~n", [Cells]),
    [binary_tree_cell(Cell) || {_, _, Cell} <- Cells],
    ok.
binary_tree_cell(Cell) ->
    %%io:format("*** C:~p~n", [Cell]),
    {ok, North} = maze_cell:north(Cell),
    {ok, East}  = maze_cell:east(Cell),
    %%io:format("*** N:~p - E:~p~n", [North, East]),
    Neighbours = [N || N <- [North, East], N =/= undefined],
    %%Index = rand:uniform(length(Neighbours)),
    %%Neighbour = lists:nth(Index, Neighbours),
    Neighbour = select_neighbours(Neighbours),
    %%io:format("*** Ns:~p - N:~p - I:~p~n", [Neighbours, Neighbour, Index]),
    %%maze_cell:link(Cell, Neighbour).
    binary_tree_cell_link(Cell, Neighbour).

select_neighbours([]) ->
    undefined;
select_neighbours([Neighbour]) ->
    Neighbour;
select_neighbours(Neighbours) ->
    Index = rand:uniform(length(Neighbours)),
    lists:nth(Index, Neighbours).


binary_tree_cell_link(_Cell, undefined) ->
    ok;
binary_tree_cell_link(Cell, Neighbour) ->
    maze_cell:link(Cell, Neighbour).

    
    

%%%-------------------------------------------------------------------
%%% End Of File
%%%-------------------------------------------------------------------
