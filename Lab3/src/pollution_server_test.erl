%%%-------------------------------------------------------------------
%%% @author Piotr
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. kwi 2019 11:53
%%%-------------------------------------------------------------------
-module(pollution_server_test).
-author("Piotr").

-include_lib("eunit/include/eunit.hrl").

start_stop_server_test() ->
  pollution_server:start(),
  timer:sleep(10),
  ?assert(lists:member(pollutionServer, registered())),
  pollution_server:stop(),
  timer:sleep(10),
  ?assertNot(lists:member(pollutionServer, registered())).

start_if_already_running_test() ->
  pollution_server:start(),
  timer:sleep(10),
  ?assertEqual(serverIsRunning, pollution_server:start()),
  pollution_server:stop().

stop_if_not_running_test() ->
  ?assertEqual(serverIsNotRunning, pollution_server:stop()).

add_station_test() ->
  ?assertEqual(serverIsNotRunning, pollution_server:addStation("Aleje", {1,2})),
  pollution_server:start(),
  ?assertEqual(stationAdded, pollution_server:addStation("Aleje", {1,2})),
  ?assertEqual(stationAdded, pollution_server:addStation("Ruczaj", {3,4})),
  ?assertEqual(existingStationError, pollution_server:addStation("Aleje", {1,2})),
  ?assertEqual(existingStationError, pollution_server:addStation("Ruczaj", {1,1})),
  ?assertEqual(existingStationError, pollution_server:addStation("Cos", {3,4})),
  pollution_server:stop().

add_get_remove_value_test() ->
  ?assertEqual(serverIsNotRunning, pollution_server:addValue({1,2}, "PM10", calendar:local_time(), 10)),
  pollution_server:start(),
  Time1 = calendar:local_time(),
  timer:sleep(1000),
  Time2 = calendar:local_time(),
  ?assertEqual(noStationError, pollution_server:addValue({1,2}, "PM10", Time1, 10)),
  pollution_server:addStation("Aleje", {1,2}),

  ?assertEqual(valueAdded, pollution_server:addValue({1,2}, "PM10", Time1, 10)),
  ?assertEqual(valueAdded, pollution_server:addValue({1,2}, "PM10", Time2, 10)),
  ?assertEqual(valueAdded, pollution_server:addValue("Aleje", "PM2.5", Time1, 10)),
  ?assertEqual(existingMeasureError, pollution_server:addValue({1,2},"PM10", Time2, 10)),
  ?assertEqual(existingMeasureError, pollution_server:addValue({1,2},"PM2.5", Time1, 15)),

  ?assertEqual(noMeasureError, pollution_server:getOneValue("Aleje", "PM15", Time1)),
  ?assertEqual(noStationError, pollution_server:getOneValue("Ruczaj", "PM15", Time1)),
  ?assertEqual(10, pollution_server:getOneValue("Aleje",  "PM10", Time1)),
  ?assertEqual(10, pollution_server:getOneValue({1,2}, "PM2.5", Time1)),

  ?assertEqual(noStationError, pollution_server:removeValue("Ruczaj","PM15", Time1)),
  ?assertEqual(valueRemoved, pollution_server:removeValue({1,2},"PM10", Time1)),
  ?assertEqual(noMeasureError, pollution_server:getOneValue("Aleje", "PM10", Time1)),
  ?assertEqual(valueRemoved, pollution_server:removeValue({1,2},"PM2.5", Time1)),
  ?assertEqual(noMeasureError, pollution_server:getOneValue("Aleje", "PM2.5", Time1)),
  pollution_server:stop().

station_mean_test() ->
  Time1 = calendar:local_time(),
  timer:sleep(1000),
  Time2 = calendar:local_time(),
  timer:sleep(1000),
  Time3 = calendar:local_time(),
  ?assertEqual(serverIsNotRunning, pollution_server:getStationMean({1,2}, "ISO")),
  pollution_server:start(),
  pollution_server:addStation("Aleje", {1,2}),
  pollution_server:addValue({1,2}, "PM10", Time1, 10),
  pollution_server:addValue({1,2}, "PM2.5", Time2, 10),
  pollution_server:addValue({1,2}, "PM2.5", Time3, 2),
  pollution_server:addValue({1,2}, "PM2.5", Time1, 3),
  pollution_server:addValue({1,2}, "PM10", Time3, 10),

  ?assertEqual(10.0, pollution_server:getStationMean("Aleje", "PM10")),
  ?assertEqual(5.0, pollution_server:getStationMean("Aleje", "PM2.5")),
  pollution_server:stop().


daily_mean_test() ->
  Time1 = calendar:local_time(),
  timer:sleep(1000),
  Time2 = calendar:local_time(),
  timer:sleep(1000),
  Time3 = calendar:local_time(),

  ?assertEqual(serverIsNotRunning, pollution_server:getDailyMean("PM10", calendar:local_time())),

  pollution_server:start(),
  pollution_server:addStation("Aleje", {1,2}),
  pollution_server:addStation("Ruczaj", {2,1}),
  pollution_server:addStation("Huta", {2,2}),

  pollution_server:addValue({1,2}, "PM10", Time1, 10),
  pollution_server:addValue({1,2}, "PM2.5", Time2, 10),
  pollution_server:addValue({1,2}, "PM2.5", Time3, 2),
  pollution_server:addValue({1,2}, "PM2.5", Time1, 3),
  pollution_server:addValue({1,2}, "PM10", Time3,10),

  pollution_server:addValue({2,1}, "PM10", Time1, 15.6),
  pollution_server:addValue({2,1}, "PM2.5", Time2, 13),
  pollution_server:addValue({2,1}, "PM2.5", Time3, 123),
  pollution_server:addValue({2,1}, "PM2.5", Time1, 0.21),
  pollution_server:addValue({2,1}, "PM10", Time3, 0.00001),

  pollution_server:addValue({2,2}, "PM10", Time1, 0.123),
  pollution_server:addValue({2,2}, "PM2.5", Time2, 69),
  pollution_server:addValue({2,2}, "PM2.5", Time3, 0.12),
  pollution_server:addValue({2,2}, "PM2.5", Time1, 3),
  pollution_server:addValue({2,2}, "PM10", Time3, 10),

  ?assertEqual((10+10+15.6+0.00001+0.123+10)/(6), pollution_server:getDailyMean("PM10", calendar:local_time())),
  ?assertEqual(24.814444444444444, pollution_server:getDailyMean("PM2.5", calendar:local_time())),
  ?assertEqual(0, pollution_server:getDailyMean("XD", calendar:local_time())),

  pollution_server:stop().
