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
    ok = observer:start(),
    example().

example() ->
    {ok, Grid1} = maze:create(sidewinder, 4, 4),
    {error, out_of_bounds} = maze_grid:at(Grid1, -1, -1),
    {error, out_of_bounds} = maze_grid:at(Grid1,  2,  4),
    {error, out_of_bounds} = maze_grid:at(Grid1,  4,  2),
    {error, out_of_bounds} = maze_grid:at(Grid1,  4,  4),
    {ok, Cell1} = maze_grid:at(Grid1, 2, 3),
    {ok, Text1} = maze_cell:to_string(Cell1),
    io:format("~s~n", [Text1]),
    {ok, Data1} = maze_grid:to_string(Grid1),
    io:format("~s~n", [Data1]),
    %%{ok, Grid2} = maze:create(foobar, 10, 10),
    %%{ok, Data2} = maze_grid:to_string(Grid2),
    %%io:format("~s~n", [Data2]),
    ok.

%%%-------------------------------------------------------------------
%%% End Of File
%%%-------------------------------------------------------------------
