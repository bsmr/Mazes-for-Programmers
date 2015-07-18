%%%-------------------------------------------------------------------
%%% @author Boris Mühmer <boris.muehmer@gmail.com>
%%% @copyright (C) 2015, Boris Mühmer
%%% @doc
%%%
%%% @end
%%% Created : 17 Jul 2015 by Boris Mühmer <boris.muehmer@gmail.com>
%%%-------------------------------------------------------------------
-module(maze_grid_srv).
-behaviour(gen_server).
-include("maze.hrl").

%% API
-export([start_link/4, to_string/1, at/3]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Maze, Mode, Rows, Columns) ->
    gen_server:start_link(?MODULE, [Maze, Mode, Rows, Columns], []).

to_string(Grid) ->
    gen_server:call(Grid, to_string).

at(Grid, Row, Column) ->
    gen_server:call(Grid, {at, Row, Column}).
    
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server.
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Maze, Mode, Rows, Columns]) ->
    {ok, #grid{maze = Maze, mode = Mode, cells = [],
	       rows = Rows, columns = Columns}, 0}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _from, #grid{cells = []} = State) ->
    Reply = {error, cells_are_not_created_yet},
    {reply, Reply, State};

handle_call(to_string, _From, #grid{rows = Rows, columns = Columns, cells = Cells} = State) ->
    CellLines = [{Row, Column, maze_cell:to_string(Cell)} || {Row, Column, Cell} <- Cells],
    Text = format_cells(CellLines, Rows, Columns),
    Reply = {ok, Text},
    {reply, Reply, State};

handle_call({at, Row, Column}, _From, #grid{rows = Rows, columns = Columns} = State)
  when (Row < 0) or (Row >= Rows) or (Column < 0) or (Column >= Columns) ->
    Reply = {error, out_of_bounds},
    {reply, Reply, State};

handle_call({at, Row, Column}, _From, #grid{cells = Cells} = State) ->
    [Cell] = [GC || {R, C, GC} <- Cells, R =:= Row, C =:= Column],
    Reply = {ok, Cell},
    {reply, Reply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(initialize, #grid{mode = Mode, rows = Rows, columns = Columns, cells = []} = State) ->
    Cells = create_grid(Rows, Columns),
    {noreply, State#grid{ cells = Cells }}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, #grid{cells = []} = State) ->
    gen_server:cast(self(), initialize),
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

create_grid(Rows, Columns) ->
    [create_grid_cell(Row, Column) || Row    <- lists:seq(0, Rows - 1),
				      Column <- lists:seq(0, Columns - 1)].

create_grid_cell(Row, Column) ->
    {ok, Cell} = maze_cell:create(self(), Row, Column),
    {Row, Column, Cell}.

%% lines = ""
%% for row = 0 to rows - 1
%%   for lin = 0 to 4
%%     for col = 0 to columns - 1
%%       lines += cell(row, col, lin)
%%     lines += newline
%%   lines += newline
%% lines
 
format_cells(Cells, Rows, Columns) ->
    format_row_col_line(Cells, 0, Rows, 0, Columns, 0, 5, "").

format_row_col_line(_Cells, Rows, Rows, _Col, _Columns, _Line, _Lines, Text) ->
    Text;
format_row_col_line(Cells, Row, Rows, Col, Columns, Lines, Lines, Text) ->
    format_row_col_line(Cells, Row + 1, Rows, 0, Columns, 0, Lines, Text);
format_row_col_line(Cells, Row, Rows, Columns, Columns, Line, Lines, Text) ->
    format_row_col_line(Cells, Row, Rows, 0, Columns, Line + 1, Lines, Text ++ "\n");
format_row_col_line(Cells, Row, Rows, Col, Columns, Line, Lines, Text) ->
    [Cell] = [X || {R, C, X} <- Cells, R == Row, C == Col],
    {ok, CLines} = Cell,
    [String] = [T || {L, T} <- CLines, L == Line],
    format_row_col_line(Cells, Row, Rows, Col + 1, Columns, Line, Lines, Text ++ String).

%%%-------------------------------------------------------------------
%%% End Of File
%%%-------------------------------------------------------------------
