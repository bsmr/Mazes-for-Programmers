%%%-------------------------------------------------------------------
%%% @author Boris Mühmer <boris.muehmer@gmail.com>
%%% @copyright (C) 2015, Boris Mühmer
%%% @doc
%%%
%%% @end
%%% Created : 18 Jul 2015 by Boris Mühmer <boris.muehmer@gmail.com>
%%%-------------------------------------------------------------------
-module(maze_grid).

-export([create/4, cells/1, configure/1, to_string/1, at/3]).

create(Maze, Mode, Rows, Columns) ->
    maze_grid_sup:start_child(Maze, Mode, Rows, Columns).

cells(Grid) ->
    maze_grid_srv:cells(Grid).

configure(Grid) ->
    maze_grid_srv:configure(Grid).

to_string(Grid) ->
    maze_grid_srv:to_string(Grid).

at(Grid, Row, Column) ->
    maze_grid_srv:at(Grid, Row, Column).

%%%-------------------------------------------------------------------
%%% End Of File
%%%-------------------------------------------------------------------
