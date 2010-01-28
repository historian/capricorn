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
    cap_cluster,
    cap_cluster_gems,
    cap_config,
    cap_event_sup,
    cap_log,
    cap_machine,
    cap_machine_apps,
    cap_machine_apps_sup,
    cap_application,
    cap_external_api,
    cap_external_apps_api,
    cap_external_gems_api,
    cap_external_machines_api,
    cap_sup,
    cap_util,
    cap_dets_updater,
    cap_internal_api,
    cap_service_users
  ]},
  {registered, [
    cap_config,
    cap_log,
    cap_primary_services,
    cap_secondary_services,
    cap_cluster,
    cap_machine,
    cap_machine_apps_sup,
    cap_external_api,
    cap_cluster_gems,
    cap_sup,
    cap_internal_api
  ]},
  {applications, [kernel, stdlib, inets, bertio, bertrpc, gcd]},
  {included_applications, [sasl]},
  {mod, {capricorn_app, [
    "/etc/capricorn/default.ini",
    "/etc/capricorn/local.ini"
  ]}}
]}.
