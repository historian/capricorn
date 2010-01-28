-module(gcd_event).


-export([notify/2, sync_notify/2]).
-export([call/3, call/4]).


notify(Pattern, Message) ->
  [begin
    #service{location=Location} = Service,
    gen_event:notify(Location, Message)
  end ||
  Service <- match_object(Pattern)].


sync_notify(Pattern, Message) ->
  [begin
    #service{location=Location} = Service,
    gen_event:sync_notify(Location, Message)
  end ||
  Service <- match_object(Pattern)].


call(Pattern, Handler, Message) ->
  [begin
    #service{location=Location} = Service,
    gen_event:call(Location, Handler, Message)
  end ||
  Service <- match_object(Pattern)].


call(Pattern, Handler, Message, Timeout) ->
  [begin
    #service{location=Location} = Service,
    gen_event:call(Location, Handler, Message, Timeout)
  end ||
  Service <- match_object(Pattern)].

