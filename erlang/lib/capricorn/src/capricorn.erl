%%
%% Start application module
%%
%% File   : capricorn.erl
%% Created: 2010-01-04
%%
%% @author simonmenke <simon.menke@gmail.com>
%% @copyright 2010 simonmenke
%%
%% @doc TODO make nice description
%%

-module(capricorn).
-author('simonmenke <simon.menke@gmail.com>').
-include("capricorn.hrl").

-export([start/0, stop/0, restart/0, reload/0]).

%% @spec start() -> ok
%% @doc Start the capricorn application
start() ->
  application:start(capricorn).

%% @spec stop() -> ok
%% @doc Stop the capricorn application
stop() ->
  application:stop(capricorn).

restart() ->
  case stop() of
  ok ->
    start();
  {error, {not_started,capricorn}} ->
    start();
  {error, Reason} ->
    {error, Reason}
  end.

reload() ->
  case supervisor:terminate_child(cap_sup, cap_config) of
  ok ->
    supervisor:restart_child(cap_sup, cap_config);
  {error, Reason} ->
    {error, Reason}
  end.
