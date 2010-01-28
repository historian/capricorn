%%
%% Supervised worker process module
%%
%% File   : cap_cluster.erl
%% Created: 2010-01-04
%%
%% @author simonmenke <simon.menke@gmail.com>
%% @copyright 2010 simonmenke
%%
%% @doc TODO make nice description
%%

-module(cap_cluster).
-author('simonmenke <simon.menke@gmail.com>').
-include("capricorn.hrl").
-behaviour(gen_server).

%% operation & maintenance api
-export([start_link/0]).
-export([all_machines/0, get_machine/1, add_machine/2, rmv_machine/1, restart_machine/1]).
 
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-record(cluster_ctx, {
  
}).

-define(SERVER, {global, ?MODULE}).

%%
%% Operation & Maintenance API
%%

%% @spec start_link() -> {ok, Pid}
%% @doc Start the cap_cluster
start_link() ->
  gen_server:start_link(?SERVER, ?MODULE, [], []).

all_machines() ->
  gen_server:call(?SERVER, {all_machines}).

get_machine(Node) ->
  gen_server:call(?SERVER, {get_machine, Node}).

add_machine(Node, Name) ->
  gen_server:call(?SERVER, {add_machine, Name, Node}).

rmv_machine(Node) ->
  gen_server:call(?SERVER, {rmv_machine, Node}).

restart_machine(Node) ->
  gen_server:call(?SERVER, {restart_machine, Node}).

%%
%% Genserver callback functions
%%

%% @spec init(State) -> {ok, State}
%% @doc Callback for initialize the cap_cluster
init([]) ->
  ets:new(cap_machines, [set, private, named_table]),
  net_kernel:monitor_nodes(true),
  {ok, #cluster_ctx{}}.

handle_call({all_machines}, _From, State) ->
  MachineNodes = ets:foldl(fun({Node, _}, Acc) ->
    [Node|Acc]
  end, [], cap_machines),
  {reply, MachineNodes, State};

handle_call({get_machine, Node}, _From, State) ->
  case ets:lookup(cap_machines, Node) of
  []           -> {reply, {error, not_found}, State};
  [{_,  Name}] -> {reply, {ok, {Node, Name}}, State}
  end;

handle_call({rmv_machine, Node}, _From, State) ->
  ets:delete(cap_machines, Node),
  {reply, ok, State};

handle_call({restart_machine, _Node}, _From, State) ->
  {reply, ok, State};

handle_call({add_machine, Name, Node}, _From, State) ->
  ets:insert(cap_machines, {Node, Name}),
  {reply, ok, State}.

%% @spec handle_cast(stop, State) -> {stop, normal, State}
%% @doc Callback for assynchronous messages
handle_cast(stop, State) ->
  {stop, normal, State};

%% @spec handle_cast(_Msg, State) -> {noreply, State}
%% @doc Callback for assynchronous messages
handle_cast(Msg, State) ->
  ?LOG_DEBUG("Unhandled cast: ~p", [Msg]),
  {noreply, State}.
 
%% @spec handle_info(_Info, State) -> {noreply, State}
%% @doc Callback for timeout or other unreconized messages
handle_info({nodeup, Node}, State) ->
  ?LOG_INFO("Machine came up: ~p", [Node]),
  {noreply, State};
handle_info({nodedown, Node}, State) ->
  ?LOG_INFO("Machine went down: ~p", [Node]),
  {noreply, State};
handle_info(Info, State) ->
  ?LOG_DEBUG("Unhandled info: ~p", [Info]),
  {noreply, State}.
 
%% @spec terminate(_Reason, _State) -> ok
%% @doc Callback for free resources used by the server
terminate(_Reason, _State) ->
  ok.
 
%% @spec code_change(_OldVsn, State, _Extra) -> {ok, State}
%% @doc Callback for upgrade source code
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
