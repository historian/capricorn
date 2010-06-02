-module(cap_external_api).
-behaviour(bertrpc).



-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).



-record(state, {
  user     :: binary() | undefined,
  services :: [atom()]
}).
-type state() :: #state{} .



-spec start_link([{atom(), atom()}]) -> {ok, pid()} .

start_link(Services) ->
  bertrpc:listen_link({local, ?MODULE}, ?MODULE, Services, 3457).



-spec init([{atom(), atom()}]) -> {ok, state()} .

init(Services) ->
  {ok, #state{ services = Services }}.



handle_call({runtime, F, A, I}, From, State) ->
  cap_external_runtime_api:handle_call({F, A, I}, From, State);

handle_call({machines, F, A, I}, From, State) ->
  cap_external_machines_api:handle_call({F, A, I}, From, State);

handle_call({gems, F, A, I}, From, State) ->
  cap_external_gems_api:handle_call({F, A, I}, From, State);

handle_call({applications, F, A, I}, From, State) ->
  cap_external_apps_api:handle_call({F, A, I}, From, State);

handle_call({M, F, A, I}, From, State) ->
  #state { services = Services } = State,
  case proplists:get_value(M, Services) of
  undefined ->
    {reply, {error, service_not_found}, State};
  Module ->
    Module:handle_call({F, A, I}, From, State)
  end.



handle_cast({runtime, F, A, I}, State) ->
  cap_external_runtime_api:handle_cast({F, A, I}, State);

handle_cast({machines, F, A, I}, State) ->
  cap_external_machines_api:handle_cast({F, A, I}, State);

handle_cast({gems, F, A, I}, State) ->
  cap_external_gems_api:handle_cast({F, A, I}, State);

handle_cast({applications, F, A, I}, State) ->
  cap_external_apps_api:handle_cast({F, A, I}, State);

handle_cast({M, F, A, I}, State) ->
  #state { services = Services } = State,
  case proplists:get_value(M, Services) of
  undefined ->
    {noreply, State};
  Module ->
    Module:handle_cast({F, A, I}, State)
  end.



handle_info(_Info, State) ->
  {noreply, State}.



terminate(_Reason, _State) ->
  ok.



code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


