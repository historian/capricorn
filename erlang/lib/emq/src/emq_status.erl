-module(emq_status).
-behaviour(gen_server).



-export([put/3, get/2, get_all/1, delete/2]).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).



-record(state, {
  name :: atom(),
  jobs :: pid()
}).
-type state() :: #state{} .



%%% External API
put(Pid, Id, Status) when is_atom(Status) ->
  put(Pid, Id, {Status, undefined});

put(Pid, Id, Message) when is_list(Message) ->
  put(Pid, Id, {undefined, Message});

put(Pid, Id, {Status, Message}) ->
  gen_server:cast(Pid, {put, {Id, Status, Message}}).



get(Pid, Id) ->
  gen_server:call(Pid, {get, Id}).



get_all(Pid) ->
  gen_server:call(Pid, {get_all}).



delete(Pid, Id) ->
  gen_server:cast(Pid, {delete, Id}).



%%% Start the server
-spec start_link(atom()) -> {ok, pid()} .
start_link(Name) ->
  gen_server:start_link(?MODULE, {Name}, []).



%%% Initialize the server
-spec init({atom()}) -> {ok, state()} .
init({Name}) ->
  Jobs = ets:new(jobs, [set, private]),
  {ok, #state{ jobs=Jobs, name=Name }}.



%%% Handle call messages
handle_call({setup_process, Name, Emq, Queue, Pool, Status}, _From, State) ->
  erlang:put('$emq_name',   Name),
  erlang:put('$emq',        Emq),
  erlang:put('$emq_pool',   Pool),
  erlang:put('$emq_queue',  Queue),
  erlang:put('$emq_status', Status),
  {reply, ok, State};

handle_call({get, Id}, _From, State) ->
  #state { jobs = Jobs } = State,
  case ets:lookup(Jobs, Id) of
  [] ->
    {reply, {error, not_found}, State};
  [{Id, Status, Message}] ->
    {reply, {ok, {Status, Message}}, State}
  end;

handle_call({get_all}, _From, State) ->
  #state { jobs = Jobs } = State,
  {reply, {ok, ets:tab2list(Jobs)}, State}.



%%% Handle cast messages
handle_cast({put, {Id, Status, Message}}, State) ->
  #state { jobs = Jobs } = State,
  Record =
  case ets:lookup(Jobs, Id) of
  [] ->
    if
    (Status == undefined) and (Message == undefined) ->
      {Id, pending, "pending"};
    (Status == undefined) ->
      {Id, pending, Message};
    (Message == undefined) ->
      {Id, Status, "pending"};
    true ->
      {Id, Status, Message}
    end;
  [{Id, OldStatus, OldMessage}] ->
    if
    (Status == undefined) and (Message == undefined) ->
      {Id, OldStatus, OldMessage};
    (Status == undefined) ->
      {Id, OldStatus, Message};
    (Message == undefined) ->
      {Id, Status, OldMessage};
    true ->
      {Id, Status, Message}
    end
  end,
  ets:insert(Jobs, Record),
  {noreply, State};

handle_cast({delete, Id}, State) ->
  #state { jobs = Jobs } = State,
  ets:delete(Jobs, Id),
  {noreply, State}.



%%% Handle generic messages
handle_info(raise_error_on_unknow_message, State) ->
  {noreply, State}.



%%% Before stopping the server
terminate(_Reason, State) ->
  #state { jobs = Jobs } = State,
  ets:delete(Jobs),
  ok.



%%% Code Changes
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



%%% Internal API


