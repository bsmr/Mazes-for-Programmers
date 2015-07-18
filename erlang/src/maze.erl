%%%-------------------------------------------------------------------
%%% @author Boris Mühmer <boris.muehmer@gmail.com>
%%% @copyright (C) 2015, Boris Mühmer
%%% @doc
%%%
%%% @end
%%% Created : 17 Jul 2015 by Boris Mühmer <boris.muehmer@gmail.com>
%%%-------------------------------------------------------------------
-module(maze).
%%-include("maze.hrl").

-export([create/3, format/2]).

format(Format, Args) ->
    lists:flatten(io_lib:format(Format, Args)).

create(Mode, Rows, Columns) ->
    maze_srv:create(Mode, Rows, Columns).

%%%===================================================================
%%% End Of File
%%%===================================================================
