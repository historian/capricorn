%%
%% Application module
%%
%% File   : capricorn_app.erl
%% Created: 2010-01-04
%%
%% @author simonmenke <simon.menke@gmail.com>
%% @copyright 2010 simonmenke
%%
%% @doc TODO make nice description
%%

-module(capricorn_app).
-author('simonmenke <simon.menke@gmail.com>').
-include("capricorn.hrl").

-behaviour(application).
 
-export([start/2, stop/1]).

-spec start(_,[string()]) -> {'error','already_started' | {'app_would_not_start','sasl'}} | {'ok',pid()}.
start(_Type, DefaultIniFiles) ->
  IniFiles = get_ini_files(DefaultIniFiles),
  case start_apps([sasl]) of
  ok              -> cap_sup:start_link(IniFiles);
  {error, Reason} -> {error, Reason}
  end.

stop(_) ->
  cap_sup:stop(),
  ok.

-spec get_ini_files([string()]) -> [string()].
get_ini_files(Default) ->
  case init:get_argument(capricorn_ini) of
  error          -> Default;
  {ok, [[]]}     -> Default;
  {ok, [Values]} -> Values
  end.

start_apps([]) ->
  ok;
start_apps([App|Rest]) ->
  case application:start(App) of
  ok                              -> start_apps(Rest);
  {error, {already_started, App}} -> start_apps(Rest);
  {error, _Reason}                -> {error, {app_would_not_start, App}}
  end.
