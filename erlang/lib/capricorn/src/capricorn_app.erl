-module(capricorn_app).

-behaviour(application).
-export([start/2,stop/1]).

start(_Type, _StartArgs) ->
  application:set_env(riak_kv, storage_backend, riak_kv_dets_backend),
  
  riak_core_util:start_app_deps(capricorn),
  
  %% Append defaults for riak_kv buckets to the bucket defaults
  %% TODO: Need to revisit this. Buckets are typically created
  %% by a specific entity; seems lame to append a bunch of unused
  %% metadata to buckets that may not be appropriate for the bucket.
  riak_core_bucket:append_bucket_defaults([]),
  
  %% Spin up supervisor
  case capricorn_sup:start_link() of
  {ok, Pid} ->
      %% Go ahead and mark the riak_kv service as up in the node watcher.
      %% The riak_core_ring_handler blocks until all vnodes have been started
      %% synchronously.
      % riak_core:register_vnode_module(cap_vnode),
      % riak_core_node_watcher:service_up(cap, self()),
      {ok, Pid};
  {error, Reason} ->
      {error, Reason}
  end.

stop(_State) ->
  ok.
