-module(cap_external_api).
-behaviour(bertrpc).

-export([start_link/1]).
-export([init/1, authenticate/1, authorize/2]).

start_link(Services) ->
  bertrpc:start_link({local, cap_external_api}, ?MODULE, Services, []).

init(OtherServices) ->
  Services = [
    {machines,
      {cap_external_machines_api,start_link,[]},
      permanent,
      1000},
    {gems,
      {cap_external_gems_api,start_link,[]},
      permanent,
      1000},
    {applications,
      {cap_external_apps_api,start_link,[]},
      permanent,
      1000}
  ],
  {ok, Services++OtherServices}.

authenticate(_User) ->
  {ok, anon}.

authorize(_User, {_M, _F}) ->
  ok.
