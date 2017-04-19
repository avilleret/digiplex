%%%-------------------------------------------------------------------
%%% @author Tony Rogvall <tony@rogvall.se>
%%% @copyright (C) 2017, Tony Rogvall
%%% @doc
%%%    Paradox digiplex protocol
%%% @end
%%% Created : 19 Apr 2017 by Tony Rogvall <tony@rogvall.se>
%%%-------------------------------------------------------------------
-module(digiplex).
-export([start/0, stop/0]).

start() ->
    application:ensure_all_started(digiplex).

stop() ->
    application:stop(digiplex).
