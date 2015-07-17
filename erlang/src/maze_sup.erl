%%%-------------------------------------------------------------------
%%% @author Boris Mühmer <boris.muehmer@gmail.com>
%%% @copyright (C) 2015, Boris Mühmer
%%% @doc
%%%
%%% @end
%%% Created : 17 Jul 2015 by Boris Mühmer <boris.muehmer@gmail.com>
%%%-------------------------------------------------------------------
-module(maze_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart intensity, and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init(_Args) ->

    SupFlags = #{strategy => one_for_one,
		 intensity => 1,
		 period => 5},

    %% maze_srv
    %% maze_grid_sup
    %% maze_cell_sup
    
    MazeServer = #{id => maze_srv,
		   start => { maze_srv, start_link, []},
		   restart => permanent,
		   shutdown => 5000,
		   type => worker,
		   modules => [ maze_srv ]},
    
    MazeGridSup = #{id => maze_grid_sup,
		    start => { maze_grid_sup, start_link, []},
		    restart => permanent,
		    shutdown => 5000,
		    type => supervisor,
		    modules => [ maze_grid_sup ]},
    
    MazeCellSup = #{id => maze_cell_sup,
		    start => { maze_cell_sup, start_link, []},
		    restart => permanent,
		    shutdown => 5000,
		    type => supervisor,
		    modules => [ maze_cell_sup ]},
    
    Children = [ MazeServer, MazeGridSup, MazeCellSup ],

    {ok, {SupFlags, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
