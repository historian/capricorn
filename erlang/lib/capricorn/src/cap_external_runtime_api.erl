-module(cap_external_runtime_api).
-export([handle_call/3, handle_cast/2]).



handle_call({nodes,
            _, _},
            _From, State) ->
  Nodes    = [atom_to_list(Node) || Node <- nodes()],
  Machines = [list_to_atom(Node) || Node <- Nodes, string:substr(Node, 1, 8) == "machine-"],
  {reply, [node() | Machines], State};



handle_call({selfupdate, [RawNode], _}, _From, State) ->
  try
    Node =
    case RawNode of
    RawNode when is_atom(RawNode)   -> RawNode;
    RawNode when is_list(RawNode)   -> list_to_atom(RawNode);
    RawNode when is_binary(RawNode) -> list_to_atom(binary_to_list(RawNode))
    end,
    {reply, cap_runtime:selfupdate(Node), State}
  catch
    throw:T -> {reply, {error, T}, State}
  end;



handle_call({reboot, [RawNode], _}, _From, State) ->
  try
    Node =
    case RawNode of
    RawNode when is_atom(RawNode)   -> RawNode;
    RawNode when is_list(RawNode)   -> list_to_atom(RawNode);
    RawNode when is_binary(RawNode) -> list_to_atom(binary_to_list(RawNode))
    end,
    {reply, cap_runtime:reboot(Node), State}
  catch
    throw:T -> {reply, {error, T}, State}
  end.



handle_cast({_, _, _},
            State) ->
  {noreply, State}.


