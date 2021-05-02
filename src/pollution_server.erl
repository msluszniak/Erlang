%%%-------------------------------------------------------------------
%%% @author Mateusz
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. Apr 2021 7:10 PM
%%%-------------------------------------------------------------------
-module(pollution_server).
-author("Mateusz").

%% API
-export([start/0, stop/0, init/0, addStation/2, addValue/4, removeValue/3, getOneValue/3, getStationMean/2, getDailyMean/2, getMaximumVariationStation/1, mapValuesWithCutoff/2, getMinOutOfMax/1, run/1, processRequest/2]).
-import(pollution, [createMonitor/0, safeDivision/1, checkIfOccurs/2, getMeasures/3, addStation/3, addValue/5, removeValue/4, getOneValue/4,
getStationMean/3, getDailyMean/3, getMaximumVariationStation/2, mapValuesWithCutoff/3, getMinOutOfMax/2]).


start() ->
  register(server, spawn(fun init/0)).

stop() ->
  server ! stop_.

processRequest(Server_Alias, Request) ->
  Server_Alias ! Request,
  receive
    A -> A
  end.

addStation(Name, Position) ->
  Pid = self(),
  processRequest(server, {addStation, Name, Position, Pid}).

addValue(Station_data, Date, Measure_type, Value) ->
  Pid = self(),
  processRequest(server, {addValue, Station_data, Date, Measure_type, Value, Pid}).

removeValue(Station_data, Date, Measure_type) ->
  Pid = self(),
  processRequest(server, {removeValue, Station_data, Date, Measure_type, Pid}).

getOneValue(Station_data, Date, Measure_type) ->
  Pid = self(),
  processRequest(server, {getOneValue, Station_data, Date, Measure_type, Pid}).

getStationMean(Date, Measure_type) ->
  Pid = self(),
  processRequest(server, {getStationMean, Date, Measure_type, Pid}).

getDailyMean(Station_data, Measure_type) ->
  Pid = self(),
  processRequest(server, {getDailyMean, Station_data, Measure_type, Pid}).

getMaximumVariationStation(Measure_type) ->
  Pid = self(),
  processRequest(server, {getMaximumVariationStation, Measure_type, Pid}).

mapValuesWithCutoff(Cutoff, Measure_type) ->
  Pid = self(),
  processRequest(server, {mapValuesWithCutoff, Cutoff, Measure_type, Pid}).

getMinOutOfMax(Measure_type) ->
  Pid = self(),
  processRequest(server, {getMinOutOfMax, Measure_type, Pid}).

init() ->
  Monitor = createMonitor(),
  run(Monitor).

run(Monitor) ->
  receive
    stop_ -> ok;
    {addStation, Name, Position, Pid} -> New_monitor = addStation(Name, Position, Monitor),
      Pid ! New_monitor,
      case New_monitor of
        {error, _} -> run(Monitor);
        _ -> run(New_monitor)
      end;

    {addValue, Station_data, Date, Measure_type, Value, Pid} -> New_monitor = addValue(Station_data, Date, Measure_type, Value, Monitor),
      Pid ! New_monitor,
      case New_monitor of
        {error, _} -> run(Monitor);
        _ -> run(New_monitor)
      end;

    {removeValue, Station_data, Date, Measure_type, Pid} -> New_monitor = removeValue(Station_data, Date, Measure_type, Monitor),
      Pid ! New_monitor,
      run(New_monitor);

    {getOneValue, Station_data, Date, Measure_type, Pid} -> Pid ! getOneValue(Station_data, Date, Measure_type, Monitor),
      run(Monitor);

    {getStationMean, Station_data, Measure_type, Pid} -> Pid ! getStationMean(Station_data, Measure_type, Monitor),
      run(Monitor);

    {getDailyMean, Station_data, Measure_type, Pid} -> Pid ! getDailyMean(Station_data, Measure_type, Monitor),
      run(Monitor);

    {getMaximumVariationStation, Measure_type, Pid} -> Pid ! getMaximumVariationStation(Measure_type, Monitor),
      run(Monitor);

    {mapValuesWithCutoff, Cutoff, Measure_type, Pid} -> New_monitor = mapValuesWithCutoff(Cutoff, Measure_type, Monitor),
      Pid ! New_monitor,
      run(New_monitor);

    {getMinOutOfMax, Measure_type, Pid} -> Pid ! getMinOutOfMax(Measure_type, Monitor),
      run(Monitor);

    _ -> io:format("Illegal argument given. To stop server use pollution_server:stop \n"),
      run(Monitor)

  end.

