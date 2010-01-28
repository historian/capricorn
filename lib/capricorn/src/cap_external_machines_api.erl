-module(cap_external_machines_api).
-behaviour(bertrpc_module).

-export([start_link/0]).
-export([init/1, bert_call/4, bert_cast/4, terminate/2, code_change/3]).

start_link() ->
  bertrpc_module:start_link(?MODULE, [], []).

init([]) ->
  {ok, state}.

bert_call(all, [], _Extra, State) ->
  Nodes    = [atom_to_list(Node) || Node <- nodes()],
  Machines = [list_to_atom(Node) || Node <- Nodes, string:substr(Node, 1, 8) == "machine-"],
  {reply, Machines, State}.

bert_cast(_, _, _Extra, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.