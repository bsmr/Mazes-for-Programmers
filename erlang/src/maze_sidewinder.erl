%%%-------------------------------------------------------------------
%%% @author Boris Mühmer <boris.muehmer@gmail.com>
%%% @copyright (C) 2015, Boris Mühmer
%%% @doc
%%%
%%% @end
%%% Created : 19 Jul 2015 by Boris Mühmer <boris.muehmer@gmail.com>
%%%-------------------------------------------------------------------
-module(maze_sidewinder).

-export([prepare/1]).

prepare(Grid) ->
    {ok, GridRows} = maze_grid:rows(Grid),
    [prepare_row(Grid, GridRow) || GridRow <- GridRows],
    ok.

prepare_row(Grid, GridRow) ->
    prepare_cell(Grid, GridRow, 0, length(GridRow), []).

prepare_cell(_Grid, _GridRow, GridRowLength, GridRowLength, _OldRun) ->
    ok;
prepare_cell(Grid, GridRow, GridRowIndex, GridRowLength, OldRun) ->
    Cell = lists:nth(GridRowIndex + 1, GridRow),
    Run = [Cell | OldRun],
    
    East  = grid_cell_dir(east, Cell),
    North = grid_cell_dir(north, Cell),
    
    AtBoundaryEast = case East of
			 undefined -> true;
			 _ -> false
		     end,
    AtBoundaryNorth = case North of
			  undefined -> true;
			  _ -> false
		      end,
    
    ShouldCloseOut = AtBoundaryEast or ((not AtBoundaryNorth) and (rand:uniform(2) == 2)),

    NextRun = case ShouldCloseOut of
		  
		  true ->
		      Member = sample(Run),
		      link(Member, grid_cell_dir(north, Member)),
		      [];
		  
		  false ->
		      link(Cell, East),
		      Run
	      end,

    prepare_cell(Grid, GridRow, GridRowIndex + 1, GridRowLength, NextRun).

grid_cell_dir(Dir, Cell) ->
    case apply(maze_cell, Dir, [Cell]) of
	{ok, DirCell} ->
	    DirCell;
	_ ->
	    undefined
    end.

link(_Cell, undefined) ->
    ok;
link(Cell, To) ->
    maze_cell:link(Cell, To).

sample(List) ->
    lists:nth(rand:uniform(length(List)), List).

%%%-------------------------------------------------------------------
%%% End Of File
%%%-------------------------------------------------------------------
