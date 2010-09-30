-module(ep2p).

-export([join/3]).

join(Node, Addresses, Cookie) ->
  case split_node(atom_to_list(Node), $@, []) of
  [Name, _] ->
    gen_server:call(ep2p_contact_list, {bootstrap, list_to_atom(Name), Addresses, Cookie}),
    gen_server:cast({?MODULE, Node}, {sync_req, node()}),
    gen_server:cast(ep2p_contact_list, {request_sync});
  _ -> false
  end,
  ok.


split_node([Chr|T], Chr, Ack) -> [lists:reverse(Ack)|split_node(T, Chr, [])];
split_node([H|T], Chr, Ack)   -> split_node(T, Chr, [H|Ack]);
split_node([], _, Ack)        -> [lists:reverse(Ack)].