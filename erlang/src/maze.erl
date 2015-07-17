%%%-------------------------------------------------------------------
%%% @author Boris Mühmer <boris.muehmer@gmail.com>
%%% @copyright (C) 2015, Boris Mühmer
%%% @doc
%%%
%%% @end
%%% Created : 17 Jul 2015 by Boris Mühmer <boris.muehmer@gmail.com>
%%%-------------------------------------------------------------------
-module(maze).

-export([start/0]).
-export([create/3, to_string/1]).

start() ->
    ok = application:start('sasl'),
    ok = application:start('maze'),
    %%ok = observer:start(),
    example().

example() ->
    {ok, Maze} = maze:create(sidewinder, 4, 4),
    {ok, Data} = maze:to_string(Maze),
    io:format("~s~n", [Data]),
    ok.
    
create(Mode, Rows, Columns) ->
    maze_srv:create(Mode, Rows, Columns).

to_string(Maze) ->
    maze_srv:to_string(Maze).

%%% End Of File
