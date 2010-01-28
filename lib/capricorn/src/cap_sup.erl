%%
%% Supervisor module
%%
%% File   : cap_sup.erl
%% Created: 2010-01-04
%%
%% @author simonmenke <simon.menke@gmail.com>
%% @copyright 2010 simonmenke
%%
%% @doc TODO make nice description
%%

-module(cap_sup).
-author('simonmenke <simon.menke@gmail.com>').
-include("capricorn.hrl").
-behaviour(supervisor).

%% operation & maintenance api
% -export([start_link/0]).
-export([start_link/1,stop/0, cap_config_start_link_wrapper/2,
         start_primary_services/1,start_secondary_services/0,
         restart_core_server/0]).

%% supervisor callback
-export([init/1]).

 
-define(SERVER, ?MODULE).
 
%%
%% Operation & Maintenance API
%%


start_link(IniFiles) ->
  case whereis(cap_sup) of
  undefined ->
    start_server(IniFiles);
  _Else ->
    {error, already_started}
  end.

restart_core_server() ->
  NodeType = list_to_atom(
    cap_config:get("capricorn", "node_type", "cluster")),
  restart_core_server(NodeType).
restart_core_server(cluster) ->
  supervisor:terminate_child(cap_primary_services, cap_cluster),
  supervisor:restart_child(cap_primary_services, cap_cluster),
  ok;
restart_core_server(machine) ->
  supervisor:terminate_child(cap_primary_services, cap_machine),
  supervisor:restart_child(cap_primary_services, cap_machine),
  ok.



cap_config_start_link_wrapper(IniFiles, FirstConfigPid) ->
  case is_process_alive(FirstConfigPid) of
  true ->
    link(FirstConfigPid),
    {ok, FirstConfigPid};
  false -> cap_config:start_link(IniFiles)
  end.

start_server(IniFiles) ->
  case init:get_argument(pidfile) of
  {ok, [PidFile]} ->
    case file:write_file(PidFile, os:getpid()) of
    ok -> ok;
    Error -> io:format("Failed to write PID file ~s, error: ~p", [PidFile, Error])
    end;
  _ -> ok
  end,
  
  {ok, ConfigPid} = cap_config:start_link(IniFiles),
  
  Node = list_to_atom(cap_config:get("capricorn", "node", "cluster")),
  case net_kernel:start([Node]) of
  {ok, _NetPid} -> ok;
  {error, Reason} ->
    ?LOG_ERROR("~s~n", [Reason]),
    throw({startup_error, Reason})
  end,
  
  Cookie = list_to_atom(capricorn_config:get("capricorn", "cookie", "secret")),
  erlang:set_cookie(node(), Cookie),
  
  NodeType = list_to_atom(
    cap_config:get("capricorn", "node_type", "cluster")),
  LogLevel = cap_config:get("log", "level", "info"),
  
  % announce startup
  io:format("Capricorn (LogLevel=~s, Node=~s, Type=~s) is starting.~n", [
    LogLevel,
    atom_to_list(node()),
    atom_to_list(NodeType)
  ]),
  
  case LogLevel of
  "debug" ->
    io:format("Configuration Settings ~p:~n", [IniFiles]),
    [io:format("  [~s] ~s=~p~n", [Module, Variable, Value])
        || {{Module, Variable}, Value} <- cap_config:all()];
  _ -> ok
  end,
  
  BaseChildSpecs =
  {{one_for_all, 10, 3600},[
    {cap_config,
      {cap_sup, cap_config_start_link_wrapper, [IniFiles, ConfigPid]},
      permanent,
      brutal_kill,
      worker,
      [cap_config]},
    {cap_primary_services,
      {cap_sup, start_primary_services, [NodeType]},
      permanent,
      infinity,
      supervisor,
      [cap_sup]},
    {cap_secondary_services,
      {cap_sup, start_secondary_services, []},
      permanent,
      infinity,
      supervisor,
      [cap_sup]}
  ]},
  
  {ok, Pid} = supervisor:start_link({local, cap_sup}, cap_sup, BaseChildSpecs),
  
  cap_config:register(fun
  ("daemons", _       ) -> ?MODULE:stop();
  ("event_handlers", _) -> ?MODULE:stop();
  ("capricorn", "node") -> ?MODULE:stop()
  end, Pid),
  
  unlink(ConfigPid),
  
  io:format("Capricorn has started. Time to relax.~n"),
  
  {ok, Pid}.

start_primary_services(cluster) ->
  ExternalApi = [
    begin
      {ok, {Module, Fun, Args}} = cap_util:parse_term(SpecStr),
      
      {list_to_atom(Name),
        {Module, Fun, Args},
        permanent,
        brutal_kill}
    end
    || {Name, SpecStr}
    <- cap_config:get("external_api"), SpecStr /= ""],
  
  supervisor:start_link({local, cap_primary_services}, cap_sup,
  {{one_for_one, 10, 3600},[
    {cap_log,
      {cap_log, start_link, []},
      permanent,
      brutal_kill,
      worker,
      [cap_log]},
    {cap_cluster,
      {cap_cluster, start_link, []},
      permanent,
      1000,
      worker,
      [cap_cluster]},
    {cap_cluster_gems,
      {cap_cluster_gems, start_link, []},
      permanent,
      1000,
      worker,
      [cap_cluster_gems]},
    {cap_events,
      {cap_events, start_link, []},
      permanent,
      brutal_kill,
      worker,
      [cap_events]},
    {cap_external_api,
      {cap_external_api, start_link, [ExternalApi]},
      permanent,
      1000,
      worker,
      [cap_external_api]}
  ]});
start_primary_services(machine) ->
  InternalApi = [
    begin
      {ok, {Module, Fun, Args}} = cap_util:parse_term(SpecStr),
      
      {list_to_atom(Name),
        {Module, Fun, Args},
        permanent,
        brutal_kill}
    end
    || {Name, SpecStr}
    <- cap_config:get("internal_api"), SpecStr /= ""],
  
  supervisor:start_link({local, cap_primary_services}, cap_sup,
  {{one_for_one, 10, 3600},[
    {cap_log,
      {cap_log, start_link, []},
      permanent,
      brutal_kill,
      worker,
      [cap_log]},
    {cap_machine,
      {cap_machine, start_link, []},
      permanent,
      1000,
      worker,
      [cap_machine]},
    {cap_machine_apps,
      {cap_machine_apps, start_link, []},
      permanent,
      1000,
      worker,
      [cap_machine_apps]},
    {cap_internal_api,
      {cap_internal_api, start_link, [InternalApi]},
      permanent,
      1000,
      worker,
      [cap_internal_api]},
    {cap_events,
      {cap_events, start_link, []},
      permanent,
      brutal_kill,
      worker,
      [cap_events]},
    {cap_machine_apps_sup,
      {cap_machine_apps_sup, start_link, []},
      permanent,
      infinity,
      supervisor,
      [cap_machine_apps_sup]}
  ]}).

start_secondary_services() ->
  DaemonChildSpecs = [
    begin
      {ok, {Module, Fun, Args}} = cap_util:parse_term(SpecStr),
      
      {list_to_atom(Name),
          {Module, Fun, Args},
          permanent,
          brutal_kill,
          worker,
          [Module]}
    end
    || {Name, SpecStr}
    <- cap_config:get("daemons"), SpecStr /= ""],
  
  EventHandlerSpecs = [
    begin
      {ok, {Module, Args}} = cap_util:parse_term(SpecStr),
      
      {list_to_atom(Name),
          {cap_event_sup, start_link, [cap_events, Module, Args]},
          permanent,
          brutal_kill,
          worker,
          [Module]}
    end
    || {Name, SpecStr}
    <- cap_config:get("event_handlers"), SpecStr /= ""],
  
  supervisor:start_link({local, cap_secondary_services}, cap_sup,
    {{one_for_one, 10, 3600}, DaemonChildSpecs++EventHandlerSpecs}).

stop() ->
  catch net_kernel:stop(), 
  catch exit(whereis(cap_sup), normal).

init(ChildSpecs) ->
  {ok, ChildSpecs}.
