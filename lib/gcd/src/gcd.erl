-module(gcd).
-include("gcd.hrl").

-export([start/0, stop/0]).

-export([cast/2, cast/3]).
-export([call/2, call/3]).
-export([connect/1]).
-export([register/2, unregister/0]).
-export([monitor/1, demonitor/1]).
-export([match_object/1, foldl/2]).


%%% Application API
start() ->
  application:start(gcd).

stop() ->
  application:stop(gcd).


%%% External API
call(Pattern, Message) ->
  [begin
    #service{location=Location} = Service,
    gen_server:call(Location, Message)
  end ||
  Service <- match_object(Pattern)].

call(Pattern, Message, Timeout) ->
  [begin
    #service{location=Location} = Service,
    gen_server:call(Location, Message, Timeout)
  end ||
  Service <- match_object(Pattern)].

cast(Pattern, Message) ->
  [begin
    #service{location=Location} = Service,
    gen_server:cast(Location, Message)
  end ||
  Service <- match_object(Pattern)].

cast(Pattern, Message, Timeout) ->
  [begin
    #service{location=Location} = Service,
    gen_server:cast(Location, Message, Timeout)
  end ||
  Service <- match_object(Pattern)].

connect(Node) when is_atom(Node) -> 
  connect([Node]);
connect([]) -> ok;
connect([Node|Rest]) ->
  net_adm:ping(Node),
  connect(Rest).

register(Name, Description) ->
  Info = erlang:process_info(self()),
  RegisteredName = proplists:get_value(registered_name, Info),
  Location =
  case RegisteredName of
  undefined -> self();
  _Else     -> {RegisteredName, node()}
  end,
  gen_server:call(gcd_srv, {register, self(), Name, node(), Location, Description}).

unregister() ->
  gen_server:call(gcd_srv, {unregister, self()}).

monitor(Pattern) ->
  gen_server:call(gcd_srv, {monitor, self(), Pattern}).

demonitor(Ref) ->
  gen_server:call(gcd_srv, {demonitor, Ref}).

foldl(Fun, Acc) ->
  gen_server:call(gcd_srv, {foldl, Fun, Acc}).

match_object(Pattern) ->
  gen_server:call(gcd_srv, {match_object, Pattern}).
