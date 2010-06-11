-module(hatter_app).
-behaviour(application).


-export([start/2, stop/1]).


start(_StartType, _) ->
  case hatter_sup:start_link() of
  {ok, Pid} -> {ok, Pid};
  Error     ->
    io:format("boot error: ~p\n", [Error]),
    Error
  end.


stop(_State) ->
  ok.

