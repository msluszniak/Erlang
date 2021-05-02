%%%-------------------------------------------------------------------
%%% @author Mateusz
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Apr 2021 7:53 PM
%%%-------------------------------------------------------------------
-module(pollution_value_collector_gen_statem).
-author("Mateusz").

-behavior(gen_statem).

-export([stop/0, set_station/1, add_value/3, store_data/0, start_link/0, init/1,
  callback_mode/0, offStation/3 , onStation/3]).
%% API

set_station(Coord) -> gen_statem:cast(station_statem, {set_station, Coord}).
add_value(Date, Measure_type, Value) -> gen_statem:cast(station_statem, {add_value, {Date, Measure_type, Value}} ).
store_data() -> gen_statem:cast(station_statem, {store_data, ok}).
stop() -> gen_statem:stop(station_statem).
start_link() ->
  io:format("hehe~n"),
  gen_statem:start_link({local, station_statem}, ?MODULE, [], []).


%% HANDLERS
callback_mode() -> state_functions.
init([]) -> {ok, offStation, []}.

offStation(_Event, {set_station, Coord}, []) ->
  io:format("hehe2 ~w~n", [Coord]),
  {next_state, onStation, {Coord, []} }.

onStation(_Event, {Msg, Args}, {Coord, List}) ->
  io:format("hehe1 ~w~n", [Msg]),
  case Msg of
    add_value -> {keep_state, {Coord, [Args | List]}};
    store_data ->
      [pollution_gen_server:addValue(Coord, Date_, Measure_type_, Value_) || {Date_, Measure_type_, Value_} <- List],
      {next_state, offStation, []}
  end.


