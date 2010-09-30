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

-spec start(_,_) -> {'error','already_started'} | {'ok',pid()} .
start(_Type, _) ->
  cap_sup:start_link().

stop(_) ->
  cap_sup:stop(),
  ok.