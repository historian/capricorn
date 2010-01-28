-module(gcd_server).


-export([cast/2]).
-export([call/2, call/3]).


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
