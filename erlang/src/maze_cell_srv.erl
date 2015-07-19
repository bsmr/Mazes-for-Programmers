%%%-------------------------------------------------------------------
%%% @author Boris Mühmer <boris.muehmer@gmail.com>
%%% @copyright (C) 2015, Boris Mühmer
%%% @doc
%%%
%%% @end
%%% Created : 17 Jul 2015 by Boris Mühmer <boris.muehmer@gmail.com>
%%%-------------------------------------------------------------------
-module(maze_cell_srv).
-behaviour(gen_server).
-include("maze.hrl").

%% API
-export([start_link/3,
	 row/1, column/1, position/1,
	 north/1, north/2,
	 south/1, south/2,
	 east/1, east/2,
	 west/1, west/2,
	 links/1,
	 link/3,
	 unlink/3,
	 is_linked/2,
	 neighbours/1]).

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
start_link(Grid, Row, Column) ->
    gen_server:start_link(?MODULE, [Grid, Row, Column], []).

row(Cell) ->
    gen_server:call(Cell, row).
column(Cell) ->
    gen_server:call(Cell, column).
position(Cell) ->
    gen_server:call(Cell, position).

north(Cell) ->
    gen_server:call(Cell, get_north).
north(Cell, North) ->
    gen_server:call(Cell, {set_north, North}).

south(Cell) ->
    gen_server:call(Cell, get_south).
south(Cell, South) ->
    gen_server:call(Cell, {set_south, South}).

east(Cell) ->
    gen_server:call(Cell, get_east).
east(Cell, East) ->
    gen_server:call(Cell, {set_east, East}).

west(Cell) ->
    gen_server:call(Cell, get_west).
west(Cell, West) ->
    gen_server:call(Cell, {set_west, West}).

links(Cell) ->
    gen_server:call(Cell, links).

link(Cell, To, Bidi) ->
    gen_server:call(Cell, {link, To, Bidi}).

unlink(Cell, To, Bidi) ->
    gen_server:call(Cell, {unlink, To, Bidi}).

is_linked(Cell, To) ->
    gen_server:call(Cell, {is_linked, To}).

neighbours(Cell) ->
    gen_server:call(Cell, neighbours).

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
init([Grid, Row, Column]) ->
    {ok, #cell{grid = Grid, row = Row, column = Column,
	       links = [],
	       north = undefined, south = undefined,
	       east = undefined, west = undefined}}.

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
handle_call(row, _From, #cell{row = Row} = State) ->
    Reply = {ok, Row},
    {reply, Reply, State};
    
handle_call(column, _From, #cell{column = Column} = State) ->
    Reply = {ok, Column},
    {reply, Reply, State};

handle_call(position, _From, #cell{row = Row, column = Column} = State) ->
    Reply = {ok, Row, Column},
    {reply, Reply, State};
    
handle_call(get_north, _From, #cell{north = Cell} = State) ->
    Reply = {ok, Cell},
    {reply, Reply, State};

handle_call(get_south, _From, #cell{south = Cell} = State) ->
    Reply = {ok, Cell},
    {reply, Reply, State};

handle_call(get_east, _From, #cell{east = Cell} = State) ->
    Reply = {ok, Cell},
    {reply, Reply, State};

handle_call(get_west, _From, #cell{west = Cell} = State) ->
    Reply = {ok, Cell},
    {reply, Reply, State};

handle_call({set_north, NewCell}, _From, #cell{north = OldCell} = State) ->
    Reply = {ok, OldCell},
    {reply, Reply, State#cell{north = NewCell}};

handle_call({set_south, NewCell}, _From, #cell{south = OldCell} = State) ->
    Reply = {ok, OldCell},
    {reply, Reply, State#cell{south = NewCell}};

handle_call({set_east, NewCell}, _From, #cell{east = OldCell} = State) ->
    Reply = {ok, OldCell},
    {reply, Reply, State#cell{east = NewCell}};

handle_call({set_west, NewCell}, _From, #cell{west = OldCell} = State) ->
    Reply = {ok, OldCell},
    {reply, Reply, State#cell{west = NewCell}};

handle_call(links, _From, #cell{links = Links} = State) ->
    %%Pids = [P || P <- Links],
    %%io:format("*** ~p:handle_call(links,...,Links:~p) => Cells:~p~n", [?MODULE, Links, Pids]),
    Reply = {ok, Links},
    {reply, Reply, State};
    
handle_call({link, To, true}, _From, #cell{links = OldLinks} = State) ->
    NewLinks = [To | OldLinks],
    %%io:format("*** [~p] ~p:handle_call({link, To:~p, true}):~n    old:~p => new:~p~n", [self(), ?MODULE, To, OldLinks, NewLinks]),
    maze_cell:link(To, self(), false),
    Reply = ok,
    {reply, Reply, State#cell{links = NewLinks}};
    
handle_call({link, To, false}, _From, #cell{links = OldLinks} = State) ->
    NewLinks = [To | OldLinks],
    %%io:format("*** [~p] ~p:handle_call({link, To:~p, false}):~n    old:~p => new:~p~n", [self(), ?MODULE, To, OldLinks, NewLinks]),
    Reply = ok,
    {reply, Reply, State#cell{links = NewLinks}};
    
handle_call({unlink, To, true}, _From, #cell{links = OldLinks} = State) ->
    NewLinks = lists:delete(To, OldLinks),
    Reply = ok,
    maze_cell:unlink(To, self(), false),
    {reply, Reply, State#cell{links = NewLinks}};
    
handle_call({unlink, To, false}, _From, #cell{links = OldLinks} = State) ->
    NewLinks = lists:delete(To, OldLinks),
    Reply = ok,
    {reply, Reply, State#cell{links = NewLinks}};
    
handle_call({is_linked, To}, _From, #cell{links = Links} = State) ->
    Linked = case [ L || L <- Links, L =:= To ] of
		 [] -> false;
		 _  -> true
	     end,
    Reply = {ok, Linked},
    {reply, Reply, State};
    
handle_call(neighbours, _From,
	    #cell{north = North, south = South, east = East, west = West} = State) ->
    Neighbours = [N || N <- [North, South, East, West], N =/= undefined],
    Reply = {ok, Neighbours},
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
%%% End Of File
%%%-------------------------------------------------------------------
