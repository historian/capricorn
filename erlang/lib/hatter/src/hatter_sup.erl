-module(hatter_sup).
-behaviour(supervisor).

-export([start_link/0, init/1]).



start_link() ->
  supervisor:start_link({local, hatter_primary_sup}, ?MODULE, {}).



init({}) ->
  {ok, {{one_for_one, 10, 3600}, [
    {hatter_config_srv,
      {hatter_config_srv, start_link, ["etc/hatter.conf"]},
      permanent,
      3000,
      worker,
      [hatter_config_srv]},
    {hatter_container_srv,
      {hatter_container_srv, start_link, []},
      permanent,
      3000,
      worker,
      [hatter_container_srv]}
  ]}}.


