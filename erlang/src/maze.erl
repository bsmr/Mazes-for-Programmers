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

start() ->
    ok = application:start('sasl'),
    ok = application:start('maze'),
    ok = observer:start(),
    ok.

%%% End Of File
