%%%-------------------------------------------------------------------
%%% @author Mateusz
%%% @copyright (C) 2021, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 30. Apr 2021 3:26 PM
%%%-------------------------------------------------------------------
-module(supervisor_).
-author("Mateusz").

-behaviour(supervisor).
-export([start/0, init/1]).

start() -> supervisor:start_link({local, ?MODULE}, ?MODULE, []).
  %unlink(whereis(?MODULE)).


init(_) ->
  io:format("no zobaczmy~n"),
  {ok,
  {#{strategy => one_for_one, intensity => 2, period => 3},
    [#{id => 'var_srv',
      start => {pollution_gen_server, start, []},
      restart => transient,
      shutdown => 2000,
      type => worker,
      modules => [pollution_gen_server]},



      #{id => 'var_collector',
        start => {pollution_value_collector_gen_statem, start_link, []},
        restart => transient,
        shutdown => 2000,
        type => worker,
        modules => [pollution_value_collector_gen_statem]}]
  }}.

%%shutdown() ->
%%  exit(whereis(?MODULE), shutdown).