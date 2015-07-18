%%%-------------------------------------------------------------------
%%% @author Boris Mühmer <boris.muehmer@gmail.com>
%%% @copyright (C) 2015, Boris Mühmer
%%% @doc
%%%
%%% @end
%%% Created : 18 Jul 2015 by Boris Mühmer <boris.muehmer@gmail.com>
%%%-------------------------------------------------------------------


-record(maze, {grids}).

-record(grid, {maze, mode, rows, columns, cells}).

-record(cell, {grid, row, column, links, north, south, east, west}).

%%%-------------------------------------------------------------------
%%% End Of File
%%%-------------------------------------------------------------------
