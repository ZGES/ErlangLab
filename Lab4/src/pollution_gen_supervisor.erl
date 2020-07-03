%%%-------------------------------------------------------------------
%%% @author Piotr
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 13. maj 2019 00:17
%%%-------------------------------------------------------------------
-module(pollution_gen_supervisor).
-behaviour(supervisor).
-author("Piotr").

%% API
-export([start_link/0, init/1, stop/0]).

start_link() ->
  _Table =ets:new(monitor_table, [public, named_table]),
  ets:insert(monitor_table, {newMonit, pollution:createMonitor()}),
  supervisor:start_link({local, supPollution}, ?MODULE, []).

init(_Args) ->
  SupFlags = #{strategy => one_for_one, intensity => 5, period => 10},
  ChildSpecs = [#{id => genPollution,
                  start => {pollution_gen_server, start, []},
                  restart => permanent,
                  shutdown => 5000,
                  type => worker,
                  modules => [pollution_gen_server]}],
  {ok, {SupFlags, ChildSpecs}}.

stop() ->
  supervisor:terminate_child(supPollution, genPollution),
  exit(supPollution, shutdown).