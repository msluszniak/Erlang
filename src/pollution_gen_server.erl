%%%-------------------------------------------------------------------
%%% @author Mateusz
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Apr 2021 2:17 PM
%%%-------------------------------------------------------------------
-module(pollution_gen_server).
-author("Mateusz").

-behaviour(gen_server).
%% API
-export([start/0, stop/0, addStation/2, addValue/4, getOneValue/3, crash/0, removeValue/3, getStationMean/2, getDailyMean/2]).
-export([init/1, handle_call/3, handle_cast/2, terminate/2, getMaximumVariationStation/1, mapValuesWithCutoff/2, getMinOutOfMax/1]).

start() -> gen_server:start_link({local, ?MODULE},?MODULE,2,[]).

init(_) ->  Monitor = pollution:createMonitor(),
  {ok, Monitor}.
stop() -> gen_server:call(?MODULE, terminate).
addStation(Name, Position) -> gen_server:call(?MODULE, {addStation, Name, Position}).
addValue(Station_data, Date, Measure_type, Value) -> gen_server:call(?MODULE, {addValue, Station_data, Date, Measure_type, Value}).
getOneValue(Station_data, Date, Measure_type) -> gen_server:call(?MODULE, {getOneValue, Station_data, Date, Measure_type}).
removeValue(Station_data, Date, Measure_type) -> gen_server:call(?MODULE, {removeValue, Station_data, Date, Measure_type}).
getStationMean(Station_data, Measure_type) -> gen_server:call(?MODULE, {getStationMean, Station_data, Measure_type}).
getDailyMean(Date, Measure_type) -> gen_server:call(?MODULE, {getDailyMean, Date, Measure_type}).
getMaximumVariationStation(Measure_type) -> gen_server:call(?MODULE, {getMaximumVariationStation, Measure_type}).
mapValuesWithCutoff(Cutoff, Measure_type) -> gen_server:call(?MODULE, {getMaximumVariationStation, Cutoff, Measure_type}).
getMinOutOfMax(Measure_type) -> gen_server:call(?MODULE, {getMinOutOfMax, Measure_type}).


crash() -> gen_server:cast(?MODULE, crash).


handle_cast(crash, Monitor) -> no:exist(), {noreply, Monitor}.

handle_call({addStation, Name, Position}, _From, Monitor) ->
  Result = pollution:addStation(Name, Position, Monitor),
  case Result of
    {error, _} = Error -> {reply, Error, Monitor};
    New_monitor -> {reply, ok, New_monitor}
  end;

handle_call({addValue, Station_data, Date, Measure_type, Value}, _From, Monitor) ->
  Result = pollution:addValue(Station_data, Date, Measure_type, Value, Monitor),
  case Result of
    {error, _} = Error -> {reply, Error, Monitor};
    New_monitor -> {reply, ok, New_monitor}
  end;


handle_call({getOneValue, Station_data, Date, Measure_type}, _From, Monitor) -> {reply, pollution:getOneValue(Station_data, Date, Measure_type, Monitor), Monitor};
handle_call({removeValue, Station_data, Date, Measure_type}, _From, Monitor) -> {reply, pollution:removeValue(Station_data, Date, Measure_type, Monitor), Monitor};
handle_call({getStationMean, Station_data, Measure_type}, _From, Monitor) -> {reply, pollution:getStationMean(Station_data, Measure_type, Monitor), Monitor};
handle_call({getDailyMean, Date, Measure_type}, _From, Monitor) -> {reply, pollution:getDailyMean(Date, Measure_type, Monitor), Monitor};
handle_call({getMaximumVariationStation, Measure_type}, _From, Monitor) -> {reply, pollution:getMaximumVariationStation(Measure_type, Monitor), Monitor};
handle_call({mapValuesWithCutoff,Cutoff, Measure_type}, _From, Monitor) -> {reply, pollution:mapValuesWithCutoff(Cutoff, Measure_type, Monitor), Monitor};
handle_call({getMinOutOfMax, Measure_type}, _From, Monitor) -> {reply, pollution:getMinOutOfMax(Measure_type, Monitor), Monitor};


handle_call(terminate,_From, Monitor) -> {stop, normal, ok, Monitor}.

terminate(normal, _) -> io:format("Process terminated.~n"), ok.

