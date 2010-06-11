-module(hare).

-export([listen/3]).

listen(Name, Callback, Options) ->
  hare_server:start_link(Name, Callback, Options).