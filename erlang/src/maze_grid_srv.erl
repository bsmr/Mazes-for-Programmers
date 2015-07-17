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

%% API
-export([start_link/3,
	 to_string/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {mode, rows, columns, cells = []}).

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
start_link(Mode, Rows, Columns) ->
    gen_server:start_link(?MODULE, [Mode, Rows, Columns], []).

to_string(Maze) ->
    gen_server:call(Maze, to_string).
    
%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Mode, Rows, Columns]) ->
    {ok, #state{mode = Mode, rows = Rows, columns = Columns}, 0}.

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
handle_call(to_string, _From, State) ->
    Reply = {ok, "soon I will render myself"},
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
handle_info(timeout, #state{ mode = Mode, rows = Rows, columns = Columns, cells = [] } = State) ->
    io:format("*** ~p:handle_info(timeout, ~p) => create cells~n", [?MODULE, State]),
    Cells = create_grid(Rows, Columns),
    io:format("~n*** Cells: ~p~n~n", [Cells]),
    {noreply, State#state{ cells = Cells }};

handle_info(timeout, State) ->
    io:format("*** ~p:handle_info(timeout, ~p)~n", [?MODULE, State]),
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
    create_grid(Rows, Columns, []).

create_grid(0, _Columns, Cells) ->
    Cells;

create_grid(Rows, Columns, Cells) ->
    [create_grid_row(Rows, Columns, Cells) | create_grid(Rows - 1, Columns, Cells)].

create_grid_row(_Row, 0, Cells) ->
    Cells;

create_grid_row(Row, Columns, Cells) ->
    [create_grid_row_column(Row, Columns, Cells) | create_grid_row(Row, Columns - 1, Cells)].

create_grid_row_column(Row, Column, Cells) ->
    io:format("*** Cell: ~p x ~p~n", [Row, Column]),
    {Row, Column, "C"}.

%%% End Of File
