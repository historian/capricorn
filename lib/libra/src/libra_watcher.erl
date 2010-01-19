-module(libra_watcher).
-include("libra.hrl").
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).


-record(state, {config,process}).


%%% External API


%%% Start the server
start_link(Config) ->
  gen_server:start_link(?MODULE, [Config], []).


%%% Initialize the server
init([Config]) ->
  libra_monitor:link(Config),
  {ok, #state{config=Config}}.


%%% Handle call messages
handle_call(_Request, _From, State) ->
  {reply, ok, State}.


%%% Handle cast messages
handle_cast(_Msg, State) ->
  {noreply, State}.


%%% Handle generic messages
handle_info({stats, Master, Children}, State) ->
  io:format("stats: ~p ~p\n", [Master, Children]),
  {noreply, State};

handle_info({process_down}, State) ->
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.


%%% Before stopping the server
terminate(_Reason, _State) ->
  ok.


%%% Code Changes
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%% Internal API
