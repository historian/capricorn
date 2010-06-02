-module(cap_runtime).
-include("capricorn.hrl").
-behaviour(gen_server).

-export([reboot/1, selfupdate/1, start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



reboot(Node) ->
  gen_server:call({cap_runtime, Node}, reboot).



selfupdate(Node) ->
  gen_server:call({cap_runtime, Node}, selfupdate).



start_link(NodeType) ->
  gen_server:start_link({local, cap_runtime}, ?MODULE, NodeType, []).



init(NodeType) ->
  {ok, NodeType}.



handle_call(selfupdate, From, NodeType) ->
  Install = os:cmd("gem update capricorn"),
  init:reboot(),
  gen_server:reply(From, list_to_binary(Install)),
  {noreply, NodeType};



handle_call(reboot, From, NodeType) ->
  ?LOG_DEBUG("rebooting", []),
  init:reboot(),
  gen_server:reply(From, ok),
  {noreply, NodeType}.



handle_cast(abcdefg, State) ->
  {noreply, State}.



handle_info({_, {data, Msg}}, State) ->
  ?LOG_DEBUG("reboot> ~s", [Msg]),
  {noreply, State}.



terminate(_Reason, _State) ->
  ok.



code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


