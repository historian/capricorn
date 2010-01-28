-module(cap_event_sup).
-behaviour(gen_server).

-include("capricorn.hrl").

-export([start_link/3,start_link/4, stop/1]).
-export([init/1, terminate/2, handle_call/3, handle_cast/2, handle_info/2,code_change/3]).

start_link(EventMgr, EventHandler, Args) ->
  gen_server:start_link(cap_event_sup, {EventMgr, EventHandler, Args}, []).

start_link(ServerName, EventMgr, EventHandler, Args) ->
  gen_server:start_link(ServerName, cap_event_sup, {EventMgr, EventHandler, Args}, []).

stop(Pid) ->
  gen_server:cast(Pid, stop).

init({EventMgr, EventHandler, Args}) ->
  ok = gen_event:add_sup_handler(EventMgr, EventHandler, Args),
  {ok, {EventMgr, EventHandler}}.

terminate(_Reason, _State) ->
  ok.

handle_call(_Whatever, _From, State) ->
  {ok, State}.

handle_cast(stop, State) ->
  {stop, normal, State}.

handle_info({gen_event_EXIT, _Handler, Reason}, State) ->
  {stop, Reason, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
