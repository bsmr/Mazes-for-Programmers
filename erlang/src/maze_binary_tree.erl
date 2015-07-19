%%%-------------------------------------------------------------------
%%% @author Boris Mühmer <boris.muehmer@gmail.com>
%%% @copyright (C) 2015, Boris Mühmer
%%% @doc
%%%
%%% @end
%%% Created : 19 Jul 2015 by Boris Mühmer <boris.muehmer@gmail.com>
%%%-------------------------------------------------------------------
-module(maze_binary_tree).

-export([prepare/1]).

prepare(Grid) ->
    {ok, Cells} = maze_grid:cells(Grid),
    [binary_tree_cell(Cell) || Cell <- Cells],
    ok.

binary_tree_cell(Cell) ->
    {ok, North} = maze_cell:north(Cell),
    {ok, East}  = maze_cell:east(Cell),
    Neighbours = [N || N <- [North, East], N =/= undefined],
    Neighbour = select_neighbours(Neighbours),
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
    ok = maze_cell:link(Cell, Neighbour).

    
%%%-------------------------------------------------------------------
%%% End Of File
%%%-------------------------------------------------------------------
