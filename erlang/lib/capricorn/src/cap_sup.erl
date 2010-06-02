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
-export([start_link/1,stop/0,
         start_primary_services/1,start_secondary_services/1,
         restart_core_server/0]).

%% supervisor callback
-export([init/1]).

 
-define(SERVER, ?MODULE).
 
%%
%% Operation & Maintenance API
%%


start_link(NodeType) ->
  case whereis(cap_sup) of
  undefined ->
    start_server(NodeType);
  _Else ->
    {error, already_started}
  end.

restart_core_server() ->
  NodeType = cap_config:get(node_type),
  restart_core_server(NodeType).
restart_core_server(cluster) ->
  supervisor:terminate_child(cap_primary_services, cap_cluster),
  supervisor:restart_child(cap_primary_services, cap_cluster),
  ok;
restart_core_server(machine) ->
  supervisor:terminate_child(cap_primary_services, cap_machine),
  supervisor:restart_child(cap_primary_services, cap_machine),
  ok.



start_server(NodeType) ->
  LogLevel = cap_config:get(log, level, info),
  
  % announce startup
  io:format("Capricorn (LogLevel=~s, Node=~s, Type=~s) is starting.~n", [
    LogLevel,
    atom_to_list(node()),
    atom_to_list(NodeType)
  ]),
  
  BaseChildSpecs =
  {{one_for_all, 10, 3600},[
    {cap_primary_services,
      {cap_sup, start_primary_services, [NodeType]},
      permanent,
      infinity,
      supervisor,
      [cap_sup]},
    {cap_secondary_services,
      {cap_sup, start_secondary_services, [NodeType]},
      permanent,
      infinity,
      supervisor,
      [cap_sup]}
  ]},
  
  {ok, Pid} = supervisor:start_link({local, cap_sup}, cap_sup, BaseChildSpecs),
  
  io:format("Capricorn has started. Time to relax.~n"),
  
  {ok, Pid}.

start_primary_services(cluster) ->
  ExternalApi = cap_config:get(cluster, api),
  
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
  InternalApi = cap_config:get(machine, api),
  
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

start_secondary_services(NodeType) ->
  DaemonChildSpecs = [
    begin
      {Name,
          {Module, Fun, Args},
          permanent,
          brutal_kill,
          worker,
          [Module]}
    end
    || {Name, {Module, Fun, Args}}
    <- cap_config:get(NodeType, daemons, [])],
  
  EventHandlerSpecs = [
    begin
      {Name,
          {cap_event_sup, start_link, [cap_events, Module, Args]},
          permanent,
          brutal_kill,
          worker,
          [Module]}
    end
    || {Name, {Module, Args}}
    <- cap_config:get(NodeType, event_handlers, [])],
  
  supervisor:start_link({local, cap_secondary_services}, cap_sup,
    {{one_for_one, 10, 3600}, DaemonChildSpecs++EventHandlerSpecs}).

stop() ->
  catch exit(whereis(cap_sup), normal).

init(ChildSpecs) ->
  {ok, ChildSpecs}.