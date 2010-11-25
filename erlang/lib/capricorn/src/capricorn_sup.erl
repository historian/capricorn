-module(capricorn_sup).

-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

-define (IF (Bool, A, B), if Bool -> A; true -> B end).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  [ webmachine_router:add_route(R)
    || R <- lists:reverse(cap_web:dispatch_table()) ],
  
  VMaster = {cap_vnode_master,
              {riak_core_vnode_master, start_link, [cap_vnode]},
              permanent,
              5000,
              worker,
              [riak_core_vnode_master]},

  % Build the process list...
  Processes = lists:flatten([
      VMaster
  ]),

  % Run the proesses...
  {ok, {{one_for_one, 10, 10}, Processes}}.
