-module(cap_web).

-export([
  dispatch_table/0
]).

dispatch_table() ->
  [{["capricorn", "nodes"], cap_web_nodes, []}].