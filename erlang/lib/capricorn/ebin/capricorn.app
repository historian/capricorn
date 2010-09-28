%%
%% Resource application file
%%
%% File   : capricorn.app
%% Created: 2010-01-04
%%
%% @author simonmenke <simon.menke@gmail.com>
%% @copyright 2010 simonmenke
%%
%% @doc TODO make nice description
%%

{application, capricorn, [
  {description, "Capricorn application version 1.0"},
  {vsn, "0.1"},
  {modules, [
    capricorn,
    capricorn_app,
    cap_application,
    cap_cluster_gems,
    cap_config,
    cap_console_dispatcher,
    cap_dets_updater,
    cap_events,
    cap_event_sup,
    cap_external_api,
    cap_external_apps_api,
    cap_external_gems_api,
    cap_external_machines_api,
    cap_external_runtime_api,
    cap_gem_utils,
    cap_log,
    cap_machine,
    cap_machine_apps,
    cap_machine_apps_sup,
    cap_runtime,
    cap_sup,
    cap_util
  ]},
  {registered, [
    cap_cluster_gems,
    cap_config,
    cap_events,
    cap_external_api,
    cap_log,
    cap_machine,
    cap_machine_apps_sup,
    cap_primary_services,
    cap_secondary_services,
    cap_sup
  ]},
  {applications, [kernel, stdlib, sasl, inets, bertio, bertrpc, emq, misultin, ejson]},
  {mod, {capricorn_app, []}}
]}.
