-module(cap_vnode).
-behaviour(riak_core_vnode).
%% API
-export([start_vnode/1]).

%% riak_core_vnode API
-export([init/1,
         terminate/2,
         handle_command/3,
         is_empty/1,
         delete/1,
         handle_handoff_command/3,
         handoff_starting/2,
         handoff_cancelled/1,
         handoff_finished/2,
         handle_handoff_data/2,
         encode_handoff_item/2]).

%% API
start_vnode(I) ->
  riak_core_vnode_master:get_vnode_pid(I, cap_vnode).

%% VNode callbacks

init([Index]) ->
  {ok, Index}.

handle_command(_, _Sender, State) ->
  {reply, ok, State}.

handle_handoff_command(_Req, _Sender, State) ->
  {reply, ok, State}.

handoff_starting(_TargetNode, State) ->
  {true, State}.

handoff_cancelled(State) ->
  {ok, State}.

handoff_finished(_TargetNode, State) ->
  {ok, State}.

handle_handoff_data(_BinObj, State) ->
  {reply, ok, State}.

encode_handoff_item(_k, _V) ->
  <<"handoff">>.

is_empty(State) ->
  {true, State}.

delete(State) ->
  {ok, State}.

terminate(_Reason, _State) ->
  ok.
