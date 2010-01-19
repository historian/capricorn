-module(capricorn_machine_apps_sup).
-include("capricorn.hrl").
-behaviour(supervisor).

-export([start_link/0, start_app_link/1, init/1]).
-export([start/1, stop/1, restart/1]).

-spec start(application()) -> any().
start(#application{id=Id}=App) ->
  supervisor:start_child(capricorn_machine_apps_sup, 
    {Id,
      {capricorn_machine_apps_sup, start_app_link, [App]},
      permanent,
      infinity,
      supervisor,
      [capricorn_machine_apps_sup]}).

-spec stop(application()) -> any().
stop(#application{id=Id}) ->
  supervisor:terminate_child(capricorn_machine_apps_sup, Id),
  supervisor:delete_child(capricorn_machine_apps_sup, Id).

-spec restart(application()) -> any().
restart(#application{id=Id}) ->
  supervisor:restart_child(capricorn_machine_apps_sup, Id).

-spec start_link() -> 'ignore' | {'error',_} | {'ok',pid()}.
start_link() ->
  Specs = {{one_for_one, 10, 3600},[]},
  case supervisor:start_link({local, capricorn_machine_apps_sup}, ?MODULE, Specs) of
  {ok, Pid} -> 
    start_children(capricorn_machine_apps:all()),
    {ok, Pid};
  E -> E
  end.

-spec start_app_link(application()) -> 'ignore' | {'error',_} | {'ok',pid()}.
start_app_link(App) ->
  Specs = {{one_for_one, 10, 3600},[
    {capricorn_application,
      {capricorn_application, start_link, [App]},
      permanent,
      1000,
      worker,
      [capricorn_application]}
  ]},
  supervisor:start_link(?MODULE, Specs).

-spec init([term()]) -> {ok, [term()]}.
init(Specs) ->
  {ok, Specs}.

-spec start_children([application()]) -> ok.
start_children([]) -> ok;
start_children([App|Rest]) ->
  start(App),
  start_children(Rest).
