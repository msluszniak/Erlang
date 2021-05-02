-module(pollution).
-author("Mateusz").
-record(monitor, {actual_id, station_map, measure_map}).

-export([createMonitor/0, addStation/3, addValue/5, removeValue/4, getOneValue/4,
  getStationMean/3, getDailyMean/3, getMaximumVariationStation/2, mapValuesWithCutoff/3, getMinOutOfMax/2, getMeasures/3, safeDivision/1, checkIfOccurs/2]).

%założyłem sobie strukturę monitora jako rekord o polach actual_id, station_map oraz measure_map. Station_map
%ma za zadanie zrzutować nazwe stacji i jej współrzędne na indywidualny nr stacji (actual_id). Następnie
%mając już identyfikator możemy otrzymać pomiary dla danej stacji odwołując się do klucza będącym id w mapie
%measure_map, która jest mapą po indeksach map (pomiarów dla stacji). Actual_id za każdym dodaniem stacji jest inkrementowane
%przez co jest indywidualne dla każej stacji.
createMonitor() ->
  #monitor{actual_id = 1, station_map = #{}, measure_map = #{}}.

safeDivision({Denominator, Nominator}) ->
  case Denominator == 0 of
    true -> "No accurate measures";
    false -> Nominator/Denominator
  end.

checkIfOccurs({Key, _}, Station_map) ->
  case Key == -1 of
    true -> "No accurate measures";
    false -> maps:get(Key, Station_map)
  end.

getMeasures(Station_data, Station_map, Measure_map) ->
  maps:get(maps:get(Station_data, Station_map), Measure_map).

addStation(Name, Position, #monitor{station_map = Station_map, measure_map = Measure_map, actual_id = Actual_id}) ->
  case (maps:is_key(Name, Station_map) or maps:is_key(Position, Station_map)) of
       true -> {error, "Error: Attempt to add station which is already in monitor"};
       false ->
         Station_map1 = Station_map#{Name => Actual_id, Position => Actual_id, Actual_id => {Name, Position}},
         Measure_map1 = Measure_map#{Actual_id => #{}},
         #monitor{actual_id = Actual_id + 1, station_map = Station_map1, measure_map = Measure_map1}
  end.

addValue(Station_data, Date, Measure_type, Value, #monitor{station_map = Station_map, measure_map = Measure_map, actual_id = Actual_id}) ->
  Measures = getMeasures(Station_data, Station_map, Measure_map),
  case maps:is_key({Date, Measure_type}, Measures)  of
      true -> {error, "Error: Attempt to add measurement which is already in monitor"};
      false ->
        Measure_map1 = Measure_map#{maps:get(Station_data, Station_map) => Measures#{{Date, Measure_type} => Value}},
        #monitor{actual_id = Actual_id, station_map = Station_map, measure_map = Measure_map1}
  end.

removeValue(Station_data, Date, Measure_type, #monitor{station_map = Station_map, measure_map = Measure_map, actual_id = Actual_id}) ->
  Measures1 = maps:remove({Date, Measure_type}, getMeasures(Station_data, Station_map, Measure_map)),
  Measure_map1 = Measure_map#{maps:get(Station_data, Station_map) => Measures1},
  #monitor{actual_id = Actual_id, station_map = Station_map, measure_map = Measure_map1}.

getOneValue(Station_data, Date, Measure_type, #monitor{station_map = Station_map, measure_map = Measure_map}) ->
  maps:get({Date, Measure_type}, getMeasures(Station_data, Station_map, Measure_map)).

getStationMean(Station_data, Measure_type, #monitor{station_map = Station_map, measure_map = Measure_map}) ->
  Mean = fun
    ({_, X}, Val, {Num, Sum}) when X == Measure_type -> {Num + 1, Sum + Val};
    (_, _, Acc) -> Acc
    end,
  safeDivision(maps:fold(Mean, {0,0}, getMeasures(Station_data, Station_map, Measure_map))).

getDailyMean(Date, Measure_type, #monitor{measure_map = Measure_map}) ->
  Station_Mean = fun
         ({{Day_date,_}, X}, Val, {Num, Sum}) when X == Measure_type, Day_date == Date -> {Num + 1, Sum + Val};
         (_, _,  Acc) -> Acc
         end,
  Sum_means = fun ({Num_station, Sum_station}, {Num_all, Sum_all}) -> {Num_all + Num_station, Sum_all + Sum_station} end,
  safeDivision(lists:foldl(Sum_means, {0, 0}, [maps:fold(Station_Mean, {0, 0}, X) || {_, X} <- maps:to_list(Measure_map)])).

getMaximumVariationStation(Measure_type, #monitor{station_map = Station_map, measure_map = Measure_map}) ->
  MinMax = fun
             ({_, X}, Val, {Min, Max}) when Val < Min, X == Measure_type -> {Val, Max};
             ({_, X}, Val, {Min, Max}) when Val > Max, X == Measure_type -> {Min, Val};
             (_,_,V) -> V
           end,
  MaxVariance = fun
                  ({Key, {Min, Max}}, {_, Acc}) when Max-Min > Acc -> {Key, Max-Min};
                  (_, {MaxVarKey, Acc}) -> {MaxVarKey, Acc}
                  end,
  checkIfOccurs(lists:foldl(MaxVariance, {-1, 0}, [{Key, maps:fold(MinMax, {0, 0}, Val)} || {Key, Val} <- maps:to_list(Measure_map)]), Station_map).

%funkcja mapuje wszystkie wartości będące poniżej pewnego cutoffa na NA (Not Available). Funkcja ta ma praktyczne zastosowanie
%gdy nasze mierniki wskazują pomiary nie mające sensu fizycznego np. steżenie pyłu ujemne, temperatury w skali Kelwina ujemne itd.
%Możemy wówczas ustawić im wszystkie wartości na not available
mapValuesWithCutoff(Cutoff, Measure_type, #monitor{station_map = Station_map, measure_map = Measure_map, actual_id = Actual_id}) ->
  Evaluator = fun
                ({_, X}, Val) when X == Measure_type, Val < Cutoff -> "NA";
                (_, Val) -> Val
                end,
  MapFun = fun (_, Map) -> maps:map(Evaluator, Map) end,
  #monitor{actual_id = Actual_id, station_map = Station_map, measure_map = maps:map(MapFun, Measure_map)}.

%funkcja zwraca stację, dla której wartość maksymalna pomiaru danego typu jest minimalna spośród wszystkich stacji
getMinOutOfMax(Measure_type, #monitor{measure_map = Measure_map, station_map = Station_map}) ->
  Max_val = fun
          ({_, X}, Val, Max) when X == Measure_type, Max < Val  -> Val;
          (_, _, Val) -> Val
        end,
  Min_val = fun
          ({Key, Val}, {_, Acc}) when Val < Acc -> {Key, Val};
          (_, {MinKey, Acc}) -> {MinKey, Acc}
        end,
  {_, Val} = checkIfOccurs(lists:foldl(Min_val, {-1, 1.0e100}, [{Key, maps:fold(Max_val,-1.0e100,Val)} || {Key, Val} <- maps:to_list(Measure_map)]), Station_map),
  Val.

%Przykładowy kod testujący
%%P = pollution:createMonitor().
%%P1 = pollution:addStation("A", 100, P).
%%P2 = pollution:addStation("B", 200, P1).
%%P3 = pollution:addStation("C",300, P2).
%%P4 = pollution:addValue("A", calendar:local_time(), "PM2,5”, 50, P3).
%%P5 = pollution:addValue("A", calendar:local_time(), "PM2,5”, 100, P4).
%%P6 = pollution:addValue("B", calendar:local_time(), "PM2,5”, 100, P5).
%%P7 = pollution:addValue("B", calendar:local_time(), "PM2,5”, 200, P6).
%%P8 = pollution:addValue("C", calendar:local_time(), "PM2,5”, 200, P7).
%%P9 = pollution:addValue("C", calendar:local_time(), "PM2,5”, 400, P8).
%%pollution:getMaximumVariationStation("PM2,5", P9).
%%pollution:getStationMean("A", "PM2,5" , P9).
%%pollution:getStationMean("A", "PM2,55" , P9).
%%pollution:getDailyMean({2021,3,27}, "PM2,5", P9). %tutaj należy poprawić na aktualną date
%%pollution:getMinOutOfMax("PM2,5", P9).
%%P10 = pollution:mapValuesWithCutoff(101, "PM2,5", P9).





