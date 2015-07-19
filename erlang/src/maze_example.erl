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
    {ok, Rows, Columns} = grid_ranks(),
    %% grid dimensions:    HEIGHT x WIDTH
    {ok, Grid} = maze:create(Rows, Columns),
    %% prepare cells for maze algorithm
    ok = maze_util:grid_configure(Grid),
    %% execute maze algorithm
    ok = maze_binary_tree:prepare(Grid),
    %% show maze
    ok = maze_util:grid_format(Grid),
    ok.

grid_ranks() ->
    case init:get_argument(gridranks) of
	{ok, Result} ->
	    [Strings] = Result,
	    Values = [string:to_integer(S) || S <- Strings],
	    Numbers = [I || {I, _R} <- Values, is_number(I)],
	    {ok, lists:nth(1, Numbers), lists:nth(2, Numbers)};
	_ ->
	    {ok, ?GRID_ROWS, ?GRID_COLUMNS}
    end.

%%%-------------------------------------------------------------------
%%% End Of File
%%%-------------------------------------------------------------------
