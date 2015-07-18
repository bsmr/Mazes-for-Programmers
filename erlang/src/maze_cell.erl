%%%-------------------------------------------------------------------
%%% @author Boris Mühmer <boris.muehmer@gmail.com>
%%% @copyright (C) 2015, Boris Mühmer
%%% @doc
%%%
%%% @end
%%% Created : 18 Jul 2015 by Boris Mühmer <boris.muehmer@gmail.com>
%%%-------------------------------------------------------------------
-module(maze_cell).

-export([create/3, to_string/1]).

create(Grid, Row, Column) ->
    maze_cell_sup:start_child(Grid, Row, Column).

to_string(Cell) ->
    maze_cell_srv:to_string(Cell).

%%%-------------------------------------------------------------------
%%% End Of File
%%%-------------------------------------------------------------------
