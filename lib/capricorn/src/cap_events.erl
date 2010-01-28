-module(cap_events).


-export([start_link/0]).
-export([notify/1, notify/2]).


start_link() ->
  gen_event:start_link({local, ?MODULE}).


notify(Msg) ->
  gen_event:notify(?MODULE, Msg).


notify(Node, Msg) when is_atom(Node) ->
  notify([Node], Msg);
  
notify([Node|Rest], Msg) ->
  gen_event:notify({?MODULE, Node}, Msg),
  notify(Rest, Msg);
  
notify([], Msg) ->
  ok.