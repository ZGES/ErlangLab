%%%-------------------------------------------------------------------
%%% @author Piotr
%%% @copyright (C) 2019, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 12. maj 2019 22:25
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-behaviour(gen_server).
-author("Piotr").

%% API
-export([start/0,stop/0,addStation/2,addValue/4,removeValue/3,getOneValue/3,getStationMean/2, getDailyMean/2, getMinimumDistanceStations/0, crash/0]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2]).

-record(monitor, {stationMap = #{}, measuresMap = #{}}).

%user interface
start() ->
  gen_server:start_link({local, genPollution}, ?MODULE, [], []).

stop() ->
  gen_server:cast(genPollution, stop).

addStation(Name, {Lang, Long}) ->
  gen_server:call(genPollution, {addStation, Name, {Lang, Long}}).

addValue(Station, Type, Date, Value) ->
  gen_server:call(genPollution, {addValue, Station, Type, Date, Value}).

removeValue(Station, Type, Date) ->
  gen_server:call(genPollution, {removeValue, Station, Type, Date}).

getOneValue(Station, Type, Date) ->
  gen_server:call(genPollution, {oneValue, Station, Type, Date}).

getStationMean(Station, Type) ->
  gen_server:call(genPollution, {stationMean, Station, Type}).

getDailyMean(Type, Date) ->
  gen_server:call(genPollution, {dailyMean, Type, Date}).

getMinimumDistanceStations() ->
  gen_server:call(genPollution, {minimumDistanceStations}).

crash() ->
  gen_server:cast(genPollution, {crash}).

%callback
init(_Arg) ->
  Monitor = hd(hd(ets:match(monitor_table, {newMonit, '$1'}))),
  {ok, Monitor}.

handle_call({addStation, Name, {Lang, Long}}, _From, Monitor) ->
  Status = pollution:addStation(Name, {Lang, Long}, Monitor),
  case is_record(Status, monitor) of
    true -> ets:insert(monitor_table, {newMonit, Status}),
            {reply, stationAdded, Status};

    _ -> {reply, Status, Monitor}
  end;
handle_call({addValue, Station, Type, Date, Value}, _From, Monitor) ->
  Status = pollution:addValue(Station, Type, Date, Value, Monitor),
  case is_record(Status, monitor) of
    true -> ets:insert(monitor_table, {newMonit, Status}),
            {reply, valueAdded, Status};

    _ -> {reply, Status, Monitor}
  end;
handle_call({removeValue, Station, Type, Date}, _From, Monitor) ->
  Status = pollution:removeValue(Station, Type, Date, Monitor),
  case is_record(Status, monitor) of
    true -> ets:insert(monitor_table, {newMonit, Status}),
            {reply, valueRemoved, Status};

    _ -> {reply, Status, Monitor}
  end;
handle_call({oneValue, Station, Type, Date}, _From, Monitor) ->
  {reply, pollution:getOneValue(Station, Type, Date, Monitor), Monitor};
handle_call({stationMean, Station, Type}, _From, Monitor) ->
  {reply, pollution:getStationMean(Station, Type, Monitor), Monitor};
handle_call({dailyMean, Type, Date}, _From, Monitor) ->
  {reply, pollution:getDailyMean(Type, Date, Monitor), Monitor};
handle_call({minimumDistanceStations}, _From, Monitor) ->
  {reply, pollution:getMinimumDistanceStations(Monitor), Monitor}.

handle_cast(stop, Monitor) ->
  {stop, normal, Monitor};
handle_cast(crash, Monitor) ->
  1/0,
  {noreply, Monitor}.

terminate(_Result, _Data) ->
  ok.