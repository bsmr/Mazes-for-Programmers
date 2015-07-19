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
-export([start_link/3, ranks/1, cells/1, at/3, rows/1]).

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
start_link(Maze, Rows, Columns) ->
    gen_server:start_link(?MODULE, [Maze, Rows, Columns], []).

ranks(Grid) ->
    gen_server:call(Grid, ranks).

cells(Grid) ->
    gen_server:call(Grid, get_cells).

at(Grid, Row, Column) ->
    gen_server:call(Grid, {at, Row, Column}).

rows(Grid) ->
    gen_server:call(Grid, get_rows).
    
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
init([Maze, Rows, Columns]) ->
    Cells = [{Row, Column, create_grid_cell(Row, Column)} ||
		Row    <- lists:seq(0, Rows - 1),
		Column <- lists:seq(0, Columns - 1)],
    {ok, #grid{maze = Maze, cells = Cells,
	       rows = Rows, columns = Columns}}.

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
handle_call(_Request, _From, #grid{cells = []} = State) ->
    Reply = {error, cells_are_not_created_yet},
    {reply, Reply, State};

handle_call(ranks, _From, #grid{rows = Rows, columns = Columns} = State) ->
    Reply = {ok, Rows, Columns},
    {reply, Reply, State};

handle_call(get_cells, _From, #grid{cells = Cells} = State) ->
    Pids = [P || {_, _, P} <- Cells],
    Reply = {ok, Pids},
    {reply, Reply, State};

handle_call(get_rows, _From, #grid{rows = Rows, cells = Cells} = State) ->
    GridRows = collect_rows_cells(0, Rows, Cells, []),
    Reply = {ok, GridRows},
    {reply, Reply, State};

handle_call({at, Row, Column}, _From, #grid{rows = Rows, columns = Columns} = State)
  when (Row < 0) or (Row >= Rows) or (Column < 0) or (Column >= Columns) ->
    Reply = {error, out_of_bounds},
    {reply, Reply, State};

handle_call({at, Row, Column}, _From, #grid{cells = Cells} = State) ->
    [Pid] = [P || {R, C, P} <- Cells, R =:= Row, C =:= Column],
    Reply = {ok, Pid},
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
handle_cast(_Msg, State) ->
    {noreply, State}.

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
handle_info(_Info, State) ->
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

%%%-------------------------------------------------------------------
%%%-------------------------------------------------------------------
create_grid_cell(Row, Column) ->
    {ok, Pid} = maze_cell:new(self(), Row, Column),
    Pid.

collect_rows_cells(Rows, Rows, _Cells, Array) ->
    Array;
collect_rows_cells(Row, Rows, Cells, Array) ->
    RowCells = collect_row_cells(Row, Cells),
    collect_rows_cells(Row + 1, Rows, Cells, [RowCells | Array]).

collect_row_cells(Row, Cells) ->
    [GRC || {R, _C, GRC} <- Cells, R == Row].

%%%-------------------------------------------------------------------
%%% End Of File
%%%-------------------------------------------------------------------
