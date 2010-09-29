-module(ep2p).

-export([join/3]).

join(Name, Addresses, Cookie) ->
  gen_server:call(ep2p_contact_list, {bootstrap, Name, Addresses, Cookie}),
  gen_server:cast({?MODULE, Name}, {sync_rec, node()}),
  gen_server:cast(ep2p_contact_list, {request_sync}),
  ok.
