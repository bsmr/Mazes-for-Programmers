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

-define(GRID_ROWS, 4).
-define(GRID_COLUMNS, 4).

start() ->
    %%ok = application:start('sasl'),
    ok = application:start('maze'),
    %%ok = observer:start(),
    example(),
    init:stop(),
    ok.

example() ->
    %% grid dimensions:       HEIGHT   x  WIDTH
    {ok, Grid} = maze:create(?GRID_ROWS, ?GRID_COLUMNS),
    %% prepare cells for maze algorithm
    ok = maze_util:grid_configure(Grid),
    %% execute maze algorithm
    ok = maze_binary_tree:prepare(Grid),
    %% show maze
    ok = maze_util:grid_format(Grid),
    ok.

%%%-------------------------------------------------------------------
%%% End Of File
%%%-------------------------------------------------------------------
