%%%-------------------------------------------------------------------
%%% @author Boris Mühmer <boris.muehmer@gmail.com>
%%% @copyright (C) 2015, Boris Mühmer
%%% @doc
%%%
%%% @end
%%% Created : 19 Jul 2015 by Boris Mühmer <boris.muehmer@gmail.com>
%%%-------------------------------------------------------------------
-module(maze_util).

-export([
	 at/3,
	 grid_configure/1,
	 cell_to_string/2,
	 grid_to_string/1,
	 grid_format/1
	]).

%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
at(Grid, Row, Column) ->
    case maze_grid:at(Grid, Row, Column) of
	{ok, Cell} ->
	    Cell;
	_ ->
	    undefined
    end.

%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
grid_configure(Grid) ->
    {ok, Cells} = maze_grid:cells(Grid),
    [cell_configure(Grid, Cell) || Cell <- Cells],
    ok.

cell_configure(Grid, Cell) ->
    {ok, Row, Column} = maze_cell:position(Cell),
    
    North = maze_util:at(Grid, Row - 1, Column),
    South = maze_util:at(Grid, Row + 1, Column),
    West  = maze_util:at(Grid, Row    , Column - 1),
    East  = maze_util:at(Grid, Row    , Column + 1),
    
    set_cell_dir(Cell, north, North),
    set_cell_dir(Cell, south, South),
    set_cell_dir(Cell, west,  West),
    set_cell_dir(Cell, east,  East),
    
    ok.

set_cell_dir(_Cell, _Dir, undefined) ->
    ok;
set_cell_dir(Cell, Dir, To) ->
    %%io:format("*** ~p:set_cell_dir(Cell:~p, Dir:~p, To:~p)~n", [?MODULE, Cell, Dir, To]),
    apply(maze_cell, Dir, [Cell, To]).

%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------

cell_to_string(Cell, Line) ->
    {ok, Links} = maze_cell:links(Cell),
    
    String = if
		 Line == 0 ->
		     {ok, North} = maze_cell:north(Cell),
		     case lists:member(North, Links) of
			 false -> "+---+";
			 true  -> "+   +"
		     end;

		 (0 < Line) and (Line < 4) ->
		     {ok, East} = maze_cell:east(Cell),
		     {ok, West} = maze_cell:west(Cell),
		     case {lists:member(West, Links), lists:member(East, Links)} of
			 {false, false} -> "|   |";
			 {false, true } -> "|    ";
			 {true,  false} -> "    |";
			 {true,  true } -> "     "
		     end;

		 Line == 4 ->
		     {ok, South} = maze_cell:south(Cell),
		     case lists:member(South, Links) of
			 false -> "+---+";
			 true  -> "+   +"
		     end;

		 %% this should not happen, but we are a good erlang
		 %% "if citizen"
		 true ->
		     "#####"

	     end,
    
    {ok, String}.

%%%-------------------------------------------------------------------
%%% convert a grid to ASCII art
%%%-------------------------------------------------------------------

%% lines = ""
%% for row = 0 to rows - 1
%%   for lin = 0 to 4
%%     for col = 0 to columns - 1
%%       lines += cell(row, col, lin)
%%     lines += newline
%%   lines += newline
%% lines
 
grid_to_string(Grid) ->
    {ok, Rows, Columns} = maze_grid:ranks(Grid),
    {ok, format_row_col_line(Grid, 0, Rows, 0, Columns, 0, 5, "")}.

format_row_col_line(_Grid, Rows, Rows, _Col, _Columns, _Line, _Lines, Text) ->
    Text;
format_row_col_line(Grid, Row, Rows, _Col, Columns, Lines, Lines, Text) ->
    format_row_col_line(Grid, Row + 1, Rows, 0, Columns, 0, Lines, Text);
format_row_col_line(Grid, Row, Rows, Columns, Columns, Line, Lines, Text) ->
    format_row_col_line(Grid, Row, Rows, 0, Columns, Line + 1, Lines, Text ++ "\n");
format_row_col_line(Grid, Row, Rows, Col, Columns, Line, Lines, Text) ->
    {ok, Cell} = maze_grid:at(Grid, Row, Col),
    {ok, String} = maze_util:cell_to_string(Cell, Line),
    format_row_col_line(Grid, Row, Rows, Col + 1, Columns, Line, Lines, Text ++ String).

%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------

grid_format(Grid) ->
    {ok, Rows, Columns} = maze_grid:ranks(Grid),
    io:format("~n=== Maze has ~p row(s) and ~p column(s) ===~n",
	      [Rows, Columns]),
    {ok, Data} = maze_util:grid_to_string(Grid),
    io:format("~s~n", [Data]),
    ok.

%%%-------------------------------------------------------------------
%%% End Of File
%%%-------------------------------------------------------------------
