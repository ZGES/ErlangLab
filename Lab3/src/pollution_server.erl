%%%-------------------------------------------------------------------
%%% @author Piotr
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 16. kwi 2019 04:05
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Piotr").

%% API
-export([start/0, stop/0,addStation/2,addValue/4,removeValue/3,getOneValue/3,getStationMean/2, getDailyMean/2, getMinimumDistanceStations/0]).

-record(monitor, {stationMap = #{}, measuresMap = #{}}).


start() ->
  case lists:member(pollutionServer, registered()) of
    true ->
      serverIsRunning;
    false ->
      register(pollutionServer, spawn(fun() -> init() end))
  end.

stop() ->
  case lists:member(pollutionServer, registered()) of
    true ->
      pollutionServer ! terminate;
    false ->
      serverIsNotRunning
  end.

init() ->
  Monitor  = pollution:createMonitor(),
  loop(Monitor).

loop(Monitor) ->
  receive
    {Pid, addStation, Name, {Lang,Long}} ->
      Status = pollution:addStation(Name,{Lang,Long}, Monitor),
      case is_record(Status, monitor) of
        true -> Pid ! {addStation, stationAdded},
          loop(Status);

        _ -> Pid ! {addStation, Status}
      end,
      loop(Monitor);
    {Pid, addValue, Station, Type, Date, Value} ->
      Status = pollution:addValue(Station, Type, Date, Value, Monitor),
      case is_record(Status, monitor) of
        true -> Pid ! {addValue, valueAdded},
          loop(Status);

        _ -> Pid ! {addValue, Status}
      end,
      loop(Monitor);
    {Pid, removeValue, Station, Type, Date} ->
      Status = pollution:removeValue(Station, Type, Date, Monitor),
      case is_record(Status, monitor) of
        true -> Pid ! {removeValue, valueRemoved},
          loop(Status);
        _ -> Pid ! {removeValue, Status}
      end,
      loop(Monitor);
    {Pid, getOneValue, Station, Type, Date} ->
      Result = pollution:getOneValue(Station, Type, Date, Monitor),
      Pid ! {oneValue, Result},
      loop(Monitor);
    {Pid, getStationMean, Station, Type} ->
      Result = pollution:getStationMean(Station, Type, Monitor),
      Pid ! {stationMean, Result},
      loop(Monitor);
    {Pid, getDailyMean, Type, Date} ->
      Result = pollution:getDailyMean(Type, Date, Monitor),
      Pid ! {dailyMean, Result},
      loop(Monitor);
    {Pid,getMinimumDistanceStations} ->
      Result = pollution:getMinimumDistanceStations(Monitor),
      Pid ! {minimumDistanceStations, Result},
      loop(Monitor);
    terminate ->
      ok
  end.

%|----------------------------------------------------------------------|
%|                           CLIENT API                                 |
%|----------------------------------------------------------------------|
addStation(Name, {Lat, Lon}) when is_number(Lat) and is_number(Lon) ->
  case lists:member(pollutionServer, registered()) of
    true ->
      pollutionServer ! {self(), addStation, Name, {Lat, Lon}},
      receive
        {addStation, Status} -> Status
      after 2000 ->
        timeout
      end;
    false ->
      serverIsNotRunning
  end.

addValue(Station, Type, Date, Value) when is_number(Value) ->
  case lists:member(pollutionServer, registered()) of
    true ->
      pollutionServer ! {self(), addValue, Station, Type, Date, Value},
      receive
        {addValue, Status} -> Status
      after 20000 ->
        timeout
      end;
    false ->
      serverIsNotRunning
  end.

removeValue(Station, Type, Date) ->
  case lists:member(pollutionServer, registered()) of
    true ->
      pollutionServer ! {self(), removeValue, Station, Type, Date},
      receive
        {removeValue, Status} -> Status
      after 20000 ->
        timeout
      end;
    false ->
      serverIsNotRunning
  end.

getOneValue(Station, Type, Date) ->
  case lists:member(pollutionServer, registered()) of
    false ->
      serverIsNotRunning;
    true ->
      pollutionServer ! {self(), getOneValue, Station, Type, Date},
      receive
        {oneValue, Value} -> Value
      after 4000 ->
        timeout
      end
  end.

getStationMean(Station, Type) ->
  case lists:member(pollutionServer, registered()) of
    false ->
      serverIsNotRunning;
    true ->
      pollutionServer ! {self(), getStationMean, Station, Type},
      receive
        {stationMean, Mean} -> Mean
      after 4000 ->
        timeout
      end
  end.

getDailyMean(Type, Date) ->
  case lists:member(pollutionServer, registered()) of
    false ->
      serverIsNotRunning;
    true ->
      pollutionServer ! {self(), getDailyMean, Type, Date},
      receive
        {dailyMean, Mean} -> Mean
      after 4000 ->
        timeout
      end
  end.

getMinimumDistanceStations() ->
  case lists:member(pollutionServer, registered()) of
    false ->
      serverIsNotRunning;
    true ->
      pollutionServer ! {self(), getMinimumDistanceStations},
      receive
        {minimumDistanceStations, Pair} -> Pair
      after 4000 ->
        timeout
      end
  end.