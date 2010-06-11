-module(hatter).

-export([stop/0, reload/0]).



stop() ->
  init:stop().



reload() ->
  {not_implemented}.


