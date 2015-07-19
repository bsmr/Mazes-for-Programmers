%%%-------------------------------------------------------------------
%%% @author Boris Mühmer <boris.muehmer@gmail.com>
%%% @copyright (C) 2015, Boris Mühmer
%%% @doc
%%%
%%% @end
%%% Created : 18 Jul 2015 by Boris Mühmer <boris.muehmer@gmail.com>
%%%-------------------------------------------------------------------
-module(maze_grid).

-export([create/3, ranks/1, cells/1, at/3, rows/1]).

create(Maze, Rows, Columns) ->
    maze_grid_sup:start_child(Maze, Rows, Columns).

ranks(Grid) ->
    maze_grid_srv:ranks(Grid).

cells(Grid) ->
    maze_grid_srv:cells(Grid).

at(Grid, Row, Column) ->
    maze_grid_srv:at(Grid, Row, Column).

rows(Grid) ->
    maze_grid_srv:rows(Grid).

%%%-------------------------------------------------------------------
%%% End Of File
%%%-------------------------------------------------------------------
