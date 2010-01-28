-module(cap_internal_api).
-behaviour(bertrpc).

-export([start_link/1]).
-export([init/1, authenticate/1, authorize/2]).

start_link(Services) ->
  bertrpc:start_link({local, cap_internal_api}, ?MODULE, Services, [{port,6789}]).

init(Services) ->
  {ok, Services}.

authenticate(_User) ->
  {ok, anon}.

authorize(_User, {_M, _F}) ->
  ok.
