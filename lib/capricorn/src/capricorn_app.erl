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

-spec start(_,_) -> {'error','already_started' | {'ok',pid()}.
start(_Type, _) ->
  NodeType = get_ini_node_type(),
  cap_sup:start_link(NodeType).

stop(_) ->
  cap_sup:stop(),
  ok.

-spec get_ini_node_type() -> machine | cluster .
get_ini_node_type() ->
  case init:get_argument(node_type) of
  error          -> machine;
  {ok, [[]]}     -> machine;
  {ok, [[Type]]} -> list_to_atom(Type)
  end.
