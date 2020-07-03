%%%-------------------------------------------------------------------
%%% @author Piotr
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. kwi 2019 13:15
%%%-------------------------------------------------------------------
-module(pollution_server_sup).
-author("Piotr").

%% API
-export([start/0, stop/0]).

start() ->
  register(serverSup, spawn(fun() ->init()end)).

init() ->
  process_flag(trap_exit, true),
  spawn_link(pollution_server, start, []),
  loop().

stop() ->
  pollutionServer ! terminate,
  serverSup ! terminate.

loop() ->
  receive
    terminate -> ok;
    {'EXIT',_,_} ->
      init(),
      loop
  end.
