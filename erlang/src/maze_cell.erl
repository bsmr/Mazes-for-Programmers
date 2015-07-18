%%%-------------------------------------------------------------------
%%% @author Boris Mühmer <boris.muehmer@gmail.com>
%%% @copyright (C) 2015, Boris Mühmer
%%% @doc
%%%
%%% @end
%%% Created : 18 Jul 2015 by Boris Mühmer <boris.muehmer@gmail.com>
%%%-------------------------------------------------------------------
-module(maze_cell).

-export([new/3,
	 row/1, column/1, position/1,
	 north/1, north/2,
	 south/1, south/2,
	 east/1, east/2,
	 west/1, west/2,
	 link/2, link/3,
	 unlink/2, unlink/3,
	 is_linked/2,
	 neighbours/1,
	 to_string/1]).

new(Grid, Row, Column) ->
    maze_cell_sup:start_child(Grid, Row, Column).

row(Cell) ->
    maze_cell_srv:row(Cell).
column(Cell) ->
    maze_cell_srv:column(Cell).
position(Cell) ->
    maze_cell_srv:position(Cell).

north(Cell) ->
    maze_cell_srv:north(Cell).
north(Cell, North) ->
    maze_cell_srv:north(Cell, North).

south(Cell) ->
    maze_cell_srv:south(Cell).
south(Cell, South) ->
    maze_cell_srv:south(Cell, South).

east(Cell) ->
    maze_cell_srv:east(Cell).
east(Cell, East) ->
    maze_cell_srv:east(Cell, East).

west(Cell) ->
    maze_cell_srv:west(Cell).
west(Cell, West) ->
    maze_cell_srv:west(Cell, West).

link(Cell, To) ->
    link(Cell, To, true).
link(Cell, To, Bidi) ->
    maze_cell_srv:link(Cell, To, Bidi).

unlink(Cell, To) ->
    unlink(Cell, To, true).
unlink(Cell, To, Bidi) ->
    maze_cell_srv:unlink(Cell, To, Bidi).

is_linked(Cell, To) ->
    maze_cell_srv:is_linked(Cell, To).

neighbours(Cell) ->
    maze_cell_src:neighbours(Cell).

to_string(Cell) ->
    maze_cell_srv:to_string(Cell).

%%%-------------------------------------------------------------------
%%% End Of File
%%%-------------------------------------------------------------------
