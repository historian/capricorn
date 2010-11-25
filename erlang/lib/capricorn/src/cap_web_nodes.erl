-module(cap_web_nodes).

%% webmachine resource exports
-export([
  init/1,
  encodings_provided/2,
  content_types_provided/2,
  produce_body/2,
  pretty_print/2
]).

%-include_lib("webmachine/include/webmachine.hrl").

-record(ctx, {}).

init(_) ->
  {ok, #ctx{}}.

encodings_provided(ReqData, Context) ->
  case wrq:method(ReqData) of
  'GET' ->
    {[
      {"identity", fun(X) -> X end},
      {"gzip", fun(X) -> zlib:gzip(X) end}
    ], ReqData, Context};
  _ ->
    {[
      {"identity", fun(X) -> X end}
    ], ReqData, Context}
  end.

content_types_provided(ReqData, Context) ->
  {[
    {"application/json", produce_body},
    {"text/plain",       pretty_print}
  ], ReqData, Context}.

produce_body(ReqData, Ctx) ->
  Body = mochijson2:encode(nodes()),
  {Body, ReqData, Ctx}.

pretty_print(RD1, C1=#ctx{}) ->
  {Json, RD2, C2} = produce_body(RD1, C1),
  {Json, RD2, C2}.
