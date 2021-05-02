%%%-------------------------------------------------------------------
%% @doc myapp top level supervisor.
%% @end
%%%-------------------------------------------------------------------

-module(myapp_sup).

-behaviour(supervisor).

-export([start_link/0]).

-export([init/1]).

-define(SERVER, ?MODULE).

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%% sup_flags() = #{strategy => strategy(),         % optional
%%                 intensity => non_neg_integer(), % optional
%%                 period => pos_integer()}        % optional
%% child_spec() = #{id => child_id(),       % mandatory
%%                  start => mfargs(),      % mandatory
%%                  restart => restart(),   % optional
%%                  shutdown => shutdown(), % optional
%%                  type => worker(),       % optional
%%                  modules => modules()}   % optional
init([]) ->
    SupFlags = #{strategy => one_for_all,
                 intensity => 0,
                 period => 1},
    ChildSpecs =
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
        modules => [pollution_value_collector_gen_statem]}],
    {ok, {SupFlags, ChildSpecs}}.

%% internal functions
