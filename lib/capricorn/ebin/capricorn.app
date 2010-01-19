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
    capricorn_cluster,
    capricorn_cluster_gems,
    capricorn_config,
    capricorn_event_sup,
    capricorn_log,
    capricorn_machine,
    capricorn_machine_apps,
    capricorn_machine_apps_sup,
    capricorn_application,
    capricorn_external_api,
    capricorn_external_apps_api,
    capricorn_external_gems_api,
    capricorn_external_machines_api,
    capricorn_sup,
    capricorn_util,
    capricorn_dets_updater,
    capricorn_internal_api
  ]},
  {registered, [
    capricorn_config,
    capricorn_log,
    capricorn_primary_services,
    capricorn_secondary_services,
    capricorn_cluster,
    capricorn_machine,
    capricorn_machine_apps_sup,
    capricorn_external_api,
    capricorn_cluster_gems,
    capricorn_sup,
    capricorn_internal_api
  ]},
  {applications, [kernel, stdlib, inets, bertio, bertrpc, gcd]},
  {included_applications, [sasl]},
  {mod, {capricorn_app, [
    "/etc/capricorn/default.ini",
    "/etc/capricorn/local.ini"
  ]}}
]}.
