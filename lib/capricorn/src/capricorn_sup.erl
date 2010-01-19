%%
%% Supervisor module
%%
%% File   : capricorn_sup.erl
%% Created: 2010-01-04
%%
%% @author simonmenke <simon.menke@gmail.com>
%% @copyright 2010 simonmenke
%%
%% @doc TODO make nice description
%%

-module(capricorn_sup).
-author('simonmenke <simon.menke@gmail.com>').
-include("capricorn.hrl").
-behaviour(supervisor).

%% operation & maintenance api
% -export([start_link/0]).
-export([start_link/1,stop/0, capricorn_config_start_link_wrapper/2,
         start_primary_services/1,start_secondary_services/0,
         restart_core_server/0]).

%% supervisor callback
-export([init/1]).

 
-define(SERVER, ?MODULE).
 
%%
%% Operation & Maintenance API
%%


start_link(IniFiles) ->
  case whereis(capricorn_sup) of
  undefined ->
    start_server(IniFiles);
  _Else ->
    {error, already_started}
  end.

restart_core_server() ->
  NodeType = list_to_atom(
    capricorn_config:get("capricorn", "node_type", "cluster")),
  restart_core_server(NodeType).
restart_core_server(cluster) ->
  supervisor:terminate_child(capricorn_primary_services, capricorn_cluster),
  supervisor:restart_child(capricorn_primary_services, capricorn_cluster),
  ok;
restart_core_server(machine) ->
  supervisor:terminate_child(capricorn_primary_services, capricorn_machine),
  supervisor:restart_child(capricorn_primary_services, capricorn_machine),
  ok.



capricorn_config_start_link_wrapper(IniFiles, FirstConfigPid) ->
  case is_process_alive(FirstConfigPid) of
  true ->
    link(FirstConfigPid),
    {ok, FirstConfigPid};
  false -> capricorn_config:start_link(IniFiles)
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
  
  {ok, ConfigPid} = capricorn_config:start_link(IniFiles),
  
  Node = list_to_atom(capricorn_config:get("capricorn", "node", "cluster")),
  case net_kernel:start([Node]) of
  {ok, _NetPid} -> ok;
  {error, Reason} ->
    ?LOG_ERROR("~s~n", [Reason]),
    throw({startup_error, Reason})
  end,
  
  NodeType = list_to_atom(
    capricorn_config:get("capricorn", "node_type", "cluster")),
  LogLevel = capricorn_config:get("log", "level", "info"),
  
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
        || {{Module, Variable}, Value} <- capricorn_config:all()];
  _ -> ok
  end,
  
  BaseChildSpecs =
  {{one_for_all, 10, 3600},[
    {capricorn_config,
      {capricorn_sup, capricorn_config_start_link_wrapper, [IniFiles, ConfigPid]},
      permanent,
      brutal_kill,
      worker,
      [capricorn_config]},
    {capricorn_primary_services,
      {capricorn_sup, start_primary_services, [NodeType]},
      permanent,
      infinity,
      supervisor,
      [capricorn_sup]},
    {capricorn_secondary_services,
      {capricorn_sup, start_secondary_services, []},
      permanent,
      infinity,
      supervisor,
      [capricorn_sup]}
  ]},
  
  {ok, Pid} = supervisor:start_link({local, capricorn_sup}, capricorn_sup, BaseChildSpecs),
  
  capricorn_config:register(fun
  ("daemons", _       ) -> ?MODULE:stop();
  ("capricorn", "node") -> ?MODULE:stop()
  end, Pid),
  
  unlink(ConfigPid),
  
  io:format("Capricorn has started. Time to relax.~n"),
  
  {ok, Pid}.

start_primary_services(cluster) ->
  ExternalApi = [
    begin
      {ok, {Module, Fun, Args}} = capricorn_util:parse_term(SpecStr),
      
      {list_to_atom(Name),
        {Module, Fun, Args},
        permanent,
        brutal_kill}
    end
    || {Name, SpecStr}
    <- capricorn_config:get("external_api"), SpecStr /= ""],
  
  supervisor:start_link({local, capricorn_primary_services}, capricorn_sup,
  {{one_for_one, 10, 3600},[
    {capricorn_log,
      {capricorn_log, start_link, []},
      permanent,
      brutal_kill,
      worker,
      [capricorn_log]},
    {capricorn_cluster,
      {capricorn_cluster, start_link, []},
      permanent,
      1000,
      worker,
      [capricorn_cluster]},
    {capricorn_cluster_gems,
      {capricorn_cluster_gems, start_link, []},
      permanent,
      1000,
      worker,
      [capricorn_cluster_gems]},
    {capricorn_external_api,
      {capricorn_external_api, start_link, [ExternalApi]},
      permanent,
      1000,
      worker,
      [capricorn_external_api]}
  ]});
start_primary_services(machine) ->
  InternalApi = [
    begin
      {ok, {Module, Fun, Args}} = capricorn_util:parse_term(SpecStr),
      
      {list_to_atom(Name),
        {Module, Fun, Args},
        permanent,
        brutal_kill}
    end
    || {Name, SpecStr}
    <- capricorn_config:get("internal_api"), SpecStr /= ""],
  
  supervisor:start_link({local, capricorn_primary_services}, capricorn_sup,
  {{one_for_one, 10, 3600},[
    {capricorn_log,
      {capricorn_log, start_link, []},
      permanent,
      brutal_kill,
      worker,
      [capricorn_log]},
    {capricorn_machine,
      {capricorn_machine, start_link, []},
      permanent,
      1000,
      worker,
      [capricorn_machine]},
    {capricorn_machine_apps,
      {capricorn_machine_apps, start_link, []},
      permanent,
      1000,
      worker,
      [capricorn_machine_apps]},
    {capricorn_internal_api,
      {capricorn_internal_api, start_link, [InternalApi]},
      permanent,
      1000,
      worker,
      [capricorn_internal_api]},
    {capricorn_machine_apps_sup,
      {capricorn_machine_apps_sup, start_link, []},
      permanent,
      infinity,
      supervisor,
      [capricorn_machine_apps_sup]}
  ]}).

start_secondary_services() ->
  DaemonChildSpecs = [
    begin
      {ok, {Module, Fun, Args}} = capricorn_util:parse_term(SpecStr),
      
      {list_to_atom(Name),
          {Module, Fun, Args},
          permanent,
          brutal_kill,
          worker,
          [Module]}
    end
    || {Name, SpecStr}
    <- capricorn_config:get("daemons"), SpecStr /= ""],
  
  supervisor:start_link({local, capricorn_secondary_services}, capricorn_sup,
    {{one_for_one, 10, 3600}, DaemonChildSpecs}).

stop() ->
  catch net_kernel:stop(), 
  catch exit(whereis(capricorn_sup), normal).

init(ChildSpecs) ->
  {ok, ChildSpecs}.
