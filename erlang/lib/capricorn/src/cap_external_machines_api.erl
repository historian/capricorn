-module(cap_external_machines_api).
-export([handle_call/3, handle_cast/2]).



handle_call({all,
            _, _},
            _From, State) ->
  Nodes    = [atom_to_list(Node) || Node <- nodes()],
  Machines = [list_to_atom(Node) || Node <- Nodes, string:substr(Node, 1, 8) == "machine-"],
  {reply, Machines, State}.



handle_cast(_, State) ->
  {noreply, State}.


