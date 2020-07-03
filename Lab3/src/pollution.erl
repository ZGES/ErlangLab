%%%-------------------------------------------------------------------
%%% @author Piotr
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 19. mar 2019 14:01
%%%-------------------------------------------------------------------
-module(pollution).
-author("Piotr").

%% API
-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4, getStationMean/3, getDailyMean/3, getMinimumDistanceStations/1]).

-record(station, {name, coords}).
-record(measure, {type, value = 0, date = calendar:local_time()}).
-record(monitor, {stationMap = #{}, measuresMap = #{}}).


createMonitor() ->
  #monitor{}.


addStation(Name, {Lang, Long}, Monitor) when is_number(Lang) and is_number(Long) and is_record(Monitor, monitor) ->
  case doesStationExist(Name, {Lang,Long}, Monitor) of
    true -> existingStationError;

    _ ->
      Station = #station{name = Name, coords = {Lang, Long}},
      NewMap = maps:put(Name, Station, Monitor#monitor.stationMap),
      NewerMap = maps:put({Lang, Long}, Station, NewMap),
      MeasMap = maps:put(Station, [], Monitor#monitor.measuresMap),
      Monitor#monitor{stationMap = NewerMap, measuresMap = MeasMap}
  end;
addStation(_, _, _) -> badarg.


addValue(Station, Type, Date, Value, Monitor) when is_number(Value) and is_record(Monitor, monitor) ->
  Flag = doesStationExist(Station, Monitor),
  if
    Flag == false -> noStationError;
    Flag == true ->
      case doesMeasureExist(Station, Type, Date, Monitor) of
        [] ->
          Measure = #measure{type = Type, value = Value, date = Date},
          StationKey = getStationFromKey(Station, Monitor),
          MeasureList = lists:append(maps:get(StationKey, Monitor#monitor.measuresMap),[Measure]),
          NewMap = maps:update(StationKey, MeasureList, Monitor#monitor.measuresMap),
          Monitor#monitor{measuresMap = NewMap};

        _ -> existingMeasureError
      end
  end;
addValue(_, _, _, _, _) -> badarg.


removeValue(Station, Type, Date, Monitor) when is_record(Monitor, monitor) ->
  Flag = doesStationExist(Station, Monitor),
  if
    Flag == false -> noStationError;
    Flag == true ->
      case doesMeasureExist(Station, Type, Date, Monitor) of
        [] -> noMeasureError;

        Measure ->
          StationKey = getStationFromKey(Station, Monitor),
          NewList = lists:delete(Measure, maps:get(StationKey, Monitor#monitor.measuresMap)),
          NewMap = maps:update(StationKey, NewList, Monitor#monitor.measuresMap),
          Monitor#monitor{measuresMap = NewMap}
      end
  end;
removeValue(_, _, _, _) -> badarg.


getOneValue(Station, Type, Date, Monitor) when is_record(Monitor, monitor)->
  Flag = doesStationExist(Station, Monitor),
  if
    Flag == false -> noStationError;
    Flag == true ->
      case doesMeasureExist(Station, Type, Date, Monitor) of
        [] -> noMeasureError;

        Measure -> Measure#measure.value
      end
  end;
getOneValue(_, _, _, _) -> badarg.


getStationMean(Station, Type, Monitor) when is_record(Monitor, monitor) ->
  Flag = doesStationExist(Station, Monitor),
  if
    Flag == false -> noStationError;
    Flag == true ->
      StationKey = getStationFromKey(Station, Monitor),
      MeasureList = maps:get(StationKey, Monitor#monitor.measuresMap),
      ResultList = lists:filter(fun(X) -> X#measure.type == Type end, MeasureList),
      calcMean(ResultList, 0 ,0)
  end;
getStationMean(_, _, _) -> badarg.


getDailyMean(Type, Date, Monitor) when is_record(Monitor, monitor)->
  StationList = maps:keys(Monitor#monitor.measuresMap),
  {{Y,R,D},{_,_,_}} = Date,
  ResultList = findElements(StationList, Type, {Y,R,D}, Monitor, []),
  calcMean(ResultList, 0, 0);
getDailyMean(_, _, _) -> badarg.



getMinimumDistanceStations(Monitor) ->
  StationList = maps:keys(Monitor#monitor.measuresMap),
  minDistHelper(StationList, 100000000, {0,0}).

minDistHelper([], _, Tuple) ->
  Tuple;
minDistHelper([H|T], MinDist, Tuple) ->
  {ActualDist, ActualTuple} = secondHelper(T, H, MinDist, Tuple),
  minDistHelper(T, ActualDist, ActualTuple).

secondHelper([], _, MinDist, Tuple) ->
  {MinDist, Tuple};
secondHelper([H|T], MainStation, MinDist, Tuple) ->
  {ThisCoordX, ThisCoordY} = H#station.coords,
  {XCoord, YCoord} = MainStation#station.coords,
  Dist = math:sqrt(math:pow(ThisCoordX - XCoord, 2) + math:pow(ThisCoordY - YCoord, 2)),
  if
    Dist < MinDist -> secondHelper(T, MainStation, Dist, {MainStation, H});
    true -> secondHelper(T, MainStation, MinDist, Tuple)
  end.


doesStationExist(Name, {Lang,Long}, Monitor) ->
  case lists:any(fun(X) -> if X == Name -> true; X == {Lang, Long} -> true; true -> false end end, maps:keys(Monitor#monitor.stationMap)) of
    true -> true;
    _ -> false
  end.

doesStationExist(Station, Monitor) ->
  case lists:any(fun(X) -> if X == Station -> true; true-> false end end, maps:keys(Monitor#monitor.stationMap)) of
    true -> true;
    _ -> false
  end.

getStationFromKey(Station, Monitor) -> maps:get(Station, Monitor#monitor.stationMap).

doesMeasureExist(Station, Type, Date, Monitor) ->
  StationKey = getStationFromKey(Station, Monitor),
  MeasureList = maps:get(StationKey, Monitor#monitor.measuresMap),
  case lists:filter(fun(X) -> case (X#measure.type == Type) and (X#measure.date == Date) of true -> true; _-> false end end, MeasureList) of
    [] -> [];
    _ -> hd(lists:filter(fun(X) -> case (X#measure.type == Type) and (X#measure.date == Date) of true -> true; _-> false end end, MeasureList))
  end.

calcMean([], Acc, Len) when Len > 0->
  Acc/Len;
calcMean([], _, _) ->
  0;
calcMean([H|T], Acc, Len) ->
  calcMean(T, Acc + H#measure.value, Len + 1).

findElements([], _, _, _, AccList) ->
  AccList;
findElements([H|T], Type, Day, Monitor, AccList) ->
  MeasuresList = maps:get(H, Monitor#monitor.measuresMap),
  findElements(T, Type, Day, Monitor, lists:append(AccList, [X || X <- MeasuresList, X#measure.type == Type, getDay(X) == Day])).

getDay(Measure) ->
  {{Y,M,D},{_,_,_}} = Measure#measure.date,
  {Y,M,D}.
