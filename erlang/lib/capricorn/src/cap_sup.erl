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
-export([start_link/0,stop/0,
         start_primary_services/0,start_secondary_services/0,
         restart_core_server/0]).

%% supervisor callback
-export([init/1]).


-define(SERVER, ?MODULE).

%%
%% Operation & Maintenance API
%%


start_link() ->
  case whereis(cap_sup) of
  undefined ->
    start_server();
  _Else ->
    {error, already_started}
  end.

restart_core_server() ->
  supervisor:terminate_child(cap_primary_services, cap_machine),
  supervisor:restart_child(cap_primary_services, cap_machine),
  ok.



start_server() ->
  {ok, ConfPid} = cap_config:start_link(),

  LogLevel = cap_config:get({node, node()}, "log.level", info),

  % announce startup
  io:format("Capricorn (LogLevel=~s, Node=~s) is starting.~n", [
    LogLevel,
    atom_to_list(node())
  ]),

  BaseChildSpecs =
  {{one_for_all, 10, 3600},[
    {cap_primary_services,
      {cap_sup, start_primary_services, []},
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

  unlink(ConfPid),

  io:format("Capricorn has started. Time to relax.~n"),

  {ok, Pid}.

start_primary_services() ->
  ConsolePort = cap_config:get({node, node()}, "console_port", 8756),

  supervisor:start_link({local, cap_primary_services}, cap_sup,
  {{one_for_one, 10, 3600},[
    {cap_config,
      {cap_config, start_link, []},
      permanent,
      1000,
      worker,
      [cap_config]},
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
      [cap_machine_apps_sup]},
    {cap_console_dispatcher,
      {cap_console_dispatcher, start_link, [ConsolePort]},
      permanent,
      1000,
      worker,
      [cap_console_dispatcher]},
    {cap_runtime,
      {cap_runtime, start_link, ["machine"]},
      permanent,
      brutal_kill,
      worker,
      [cap_runtime]}
  ]}).

start_secondary_services() ->
  % DaemonChildSpecs = [
  %   begin
  %     {Name,
  %         {Module, Fun, Args},
  %         permanent,
  %         brutal_kill,
  %         worker,
  %         [Module]}
  %   end
  %   || {Name, {Module, Fun, Args}}
  %   <- cap_config:get(NodeType, daemons, [])],
  %
  % EventHandlerSpecs = [
  %   begin
  %     {Name,
  %         {cap_event_sup, start_link, [cap_events, Module, Args]},
  %         permanent,
  %         brutal_kill,
  %         worker,
  %         [Module]}
  %   end
  %   || {Name, {Module, Args}}
  %   <- cap_config:get(NodeType, event_handlers, [])],

  supervisor:start_link({local, cap_secondary_services}, cap_sup,
    {{one_for_one, 10, 3600}, []}).

stop() ->
  catch exit(whereis(cap_sup), normal).

init(ChildSpecs) ->
  {ok, ChildSpecs}.
