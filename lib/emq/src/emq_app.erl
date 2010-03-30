-module(emq_app).
-behaviour(application).
 
-export([start/2, stop/1]).

start(_Type, _Args) ->
  emq_sup:start_link().

stop(_) ->
  emq_sup:stop(),
  ok.
