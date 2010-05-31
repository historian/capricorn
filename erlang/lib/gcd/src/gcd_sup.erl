-module(gcd_sup).
-behaviour(supervisor).

-export([start_link/0]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, gcd_sup}, ?MODULE, []).

init([]) ->
  {ok, {{one_for_one, 10, 3600},[
    {gcd_srv,
      {gcd_srv, start_link, []},
      permanent,
      brutal_kill,
      worker,
      [gcd_srv]}
  ]}}.
