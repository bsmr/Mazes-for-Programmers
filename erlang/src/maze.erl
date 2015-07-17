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
    {ok, Maze1} = maze:create(sidewinder, 4, 4),
    {ok, Data1} = maze:to_string(Maze1),
    io:format("~s~n", [Data1]),
    %%{ok, Maze2} = maze:create(foobar, 10, 10),
    %%{ok, Data2} = maze:to_string(Maze2),
    %%io:format("~s~n", [Data2]),
    ok.
    
create(Mode, Rows, Columns) ->
    maze_srv:create(Mode, Rows, Columns).

to_string(Maze) ->
    maze_srv:to_string(Maze).

%%% End Of File
