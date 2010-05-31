-module(emq_queue).
-behaviour(gen_server).



-export([size/1, push/6, reserve/1, commit/2, rollback/2]).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).



-record(state, {
  name     :: atom(),
  jobs     :: pid(),
  queue    :: queue(),
  reserved :: [term()]
}).
-type state() :: #state{} .



%%% External API
size(Pid) ->
  gen_server:call(Pid, {size}).



push(Pid, Id, Dependencies, Dependents, Callbacks, Job) ->
  gen_server:cast(Pid, {push, Id, Dependencies, Dependents, Callbacks, Job}).



reserve(Pid) ->
  gen_server:call(Pid, {reserve}).



commit(Pid, Id) ->
  gen_server:cast(Pid, {commit, Id}).



rollback(Pid, Id) ->
  gen_server:cast(Pid, {rollback, Id}).



%%% Start the server
-spec start_link(atom()) -> {ok, pid()} .
start_link(Name) ->
  gen_server:start_link(?MODULE, {Name}, []).



%%% Initialize the server
-spec init({atom()}) -> {ok, state()} .
init({Name}) ->
  Jobs = ets:new(queue_jobs, [set, private]),
  {ok, #state{ jobs=Jobs, queue=queue:new(), name = Name, reserved = [] }}.



%%% Handle call messages
handle_call({setup_process, Name, Emq, Queue, Pool, Status}, _From, State) ->
  erlang:put('$emq_name',   Name),
  erlang:put('$emq',        Emq),
  erlang:put('$emq_pool',   Pool),
  erlang:put('$emq_queue',  Queue),
  erlang:put('$emq_status', Status),
  {reply, ok, State};

handle_call({size}, _From, State) ->
  #state { jobs=Jobs } = State,
  {reply, ets:info(Jobs, size), State};

handle_call({reserve}, From, State) ->
  #state { jobs = Jobs, queue = Queue1, reserved = Reserved } = State,
  case queue:out(Queue1) of
  {{value, Id}, Queue2} ->
    State2 = State#state { queue = Queue2 },
    case ets:lookup(Jobs, Id) of
    [] ->
      handle_call({reserve}, From, State2);
    [{Id, _Dependencies, _Dependents, Callbacks, Job}] ->
      State3 = State2#state { reserved = [Id|Reserved] },
      {reply, {value, Id, Job, Callbacks}, State3}
    end;
  {empty, _Queue} ->
    {reply, {empty}, State}
  end.



%%% Handle cast messages;
handle_cast({commit, Id}, State1) ->
  #state { jobs=Jobs, reserved = Reserved1, name = Name } = State1,
  Reserved2 = lists:delete(Id, Reserved1),
  State2    = State1#state { reserved = Reserved2 },
  case ets:lookup(Jobs, Id) of
  [] -> 
    {noreply, State2};
  [{Id, _, Dependents, _, _}] ->
    ets:delete(Jobs, Id),
    
    State3 = lists:foldl(fun
    ({QueueName, DependentId}, OldState)->
      case emq_sup:get_queue(QueueName) of
      {ok, QueuePid} when QueuePid == self() ->
        {noreply, NewState} =
        handle_info({remove_dependency, DependentId, {Name, Id}}, OldState),
        NewState;
      {ok, QueuePid} ->
        QueuePid ! {remove_dependency, DependentId, {Name, Id}},
        OldState;
      {error, not_found} ->
        OldState
      end
    end, State2, Dependents),
    
    {noreply, State3}
  end;

handle_cast({rollback, Id}, State1) ->
  #state { queue = Queue1, reserved = Reserved1 } = State1,
  Reserved2 = lists:delete(Id, Reserved1),
  Queue2    = queue:in(Id, Queue1),
  State2    = State1#state { queue = Queue2, reserved = Reserved2 },
  {noreply, State2};

handle_cast({push, Id1, Dependencies1, Dependents1, Callbacks1, Job1}, State1) ->
  #state { jobs = Jobs } = State1,
  case ets:lookup(Jobs, Id1) of
  [] ->
    handle_cast({push_new, Id1, Dependencies1, Dependents1, Callbacks1, Job1}, State1);
  [{Id1, Dependencies2, Dependents2, Callbacks2, Job2}] ->
    handle_cast({push_update, {Id1, Dependencies2, Dependents2, Callbacks2, Job2}, {Dependencies1, Dependents1, Callbacks1}}, State1)
  end;

handle_cast({push_update, {Id, Dependencies, Dependents, Callbacks, Job}, {NewDependencies, NewDependents, NewCallbacks}}, State1) ->
  #state { jobs = Jobs, name = Name } = State1,
  
  CombinedDependencies = Dependencies ++ NewDependencies,
  CombinedDependents   = Dependents ++ NewDependents,
  CombinedCallbacks    = Callbacks ++ NewCallbacks,
  
  ets:insert(Jobs, {Id, CombinedDependencies, CombinedDependents, CombinedCallbacks, Job}),
  
  State2 = lists:foldl(fun
  ({QueueName, DependentId}, OldState)->
    case emq_sup:get_queue(QueueName) of
    {ok, QueuePid} when QueuePid == self() ->
      {noreply, NewState} =
      handle_info({new_dependency, DependentId, {Name, Id}}, OldState),
      NewState;
    {ok, QueuePid} ->
      QueuePid ! {new_dependency, DependentId, {Name, Id}},
      OldState;
    {error, not_found} ->
      OldState
    end
  end, State1, NewDependents),
  
  State3 = lists:foldl(fun
  ({QueueName, DependencyId}, OldState)->
    case emq_sup:get_queue(QueueName) of
    {ok, QueuePid} when QueuePid == self() ->
      {noreply, NewState} =
      handle_info({new_dependent, DependencyId, {Name, Id}}, OldState),
      NewState;
    {ok, QueuePid} ->
      QueuePid ! {new_dependent, DependencyId, {Name, Id}},
      OldState;
    {error, not_found} ->
      OldState
    end
  end, State2, NewDependencies),
  
  #state { queue = Queue1, reserved = Reserved } = State3,
  
  Queue2 = queue:filter(fun
  (QueuedId) when QueuedId == Id -> false;
  (_) -> true
  end, Queue1),
  
  {ok, StatusPid} = emq_sup:get_status(local),
  
  case CombinedDependencies of
  [] ->
    case lists:member(Id, Reserved) of
    true -> 
      {noreply, State3#state { queue = Queue2 }};
    false -> 
      Queue3 = queue:in(Id, Queue2),
      emq_status:put(StatusPid, Id, queued),
      {noreply, State3#state { queue = Queue3 }}
    end;
  _Else ->
    emq_status:put(StatusPid, Id, pending),
    {noreply, State3#state { queue = Queue2 }}
  end;

handle_cast({push_new, Id, Dependencies, Dependents, Callbacks, Job}, State1) ->
  #state { jobs = Jobs, name = Name } = State1,
  
  ets:insert(Jobs, {Id, Dependencies, Dependents, Callbacks, Job}),
  
  State2 = lists:foldl(fun
  ({QueueName, DependentId}, OldState)->
    case emq_sup:get_queue(QueueName) of
    {ok, QueuePid} when QueuePid == self() ->
      {noreply, NewState} =
      handle_info({new_dependency, DependentId, {Name, Id}}, OldState),
      NewState;
    {ok, QueuePid} ->
      QueuePid ! {new_dependency, DependentId, {Name, Id}},
      OldState;
    {error, not_found} ->
      OldState
    end
  end, State1, Dependents),
  
  
  State3 = lists:foldl(fun
  ({QueueName, DependencyId}, OldState)->
    case emq_sup:get_queue(QueueName) of
    {ok, QueuePid} when QueuePid == self() ->
      {noreply, NewState} =
      handle_info({new_dependent, DependencyId, {Name, Id}}, OldState),
      NewState;
    {ok, QueuePid} ->
      QueuePid ! {new_dependent, DependencyId, {Name, Id}},
      OldState;
    {error, not_found} ->
      OldState
    end
  end, State2, Dependencies),
  
  
  {ok, StatusPid} = emq_sup:get_status(local),
  
  
  case Dependencies of
  [] -> 
    #state { queue = Queue1 } = State3,
    Queue2 = queue:in(Id, Queue1),
    emq_status:put(StatusPid, Id, queued),
    {noreply, State3#state { queue = Queue2 }};
  _Else ->
    emq_status:put(StatusPid, Id, pending),
    {noreply, State3}
  end.



%%% Handle generic messages
handle_info({new_dependent, DependencyId, {Queue, Id}}, State) ->
  #state { jobs = Jobs, name = Name } = State,
  case ets:lookup(Jobs, DependencyId) of
  [] ->
    case emq_sup:get_queue(Queue) of
    {ok, QueuePid} when QueuePid == self() ->
      handle_info({remove_dependency, Id, {Name, DependencyId}}, State);
    {ok, QueuePid} ->
      QueuePid ! {remove_dependency, Id, {Name, DependencyId}},
      {noreply, State};
    {error, not_found} ->
      {noreply, State}
    end;
  [{DependencyId, Dependencies, Dependents, Callbacks, Job}] ->
    ets:insert(Jobs, {DependencyId, Dependencies, [{Queue, Id}|Dependents], Callbacks, Job}),
    {noreply, State}
  end;

handle_info({new_dependency, DependentId, {Queue, Id}}, State) ->
  #state { jobs = Jobs, queue = Queue1 } = State,
  case ets:lookup(Jobs, DependentId) of
  [] ->
    {noreply, State};
  [{DependentId, Dependencies, Dependents, Callbacks, Job}] ->
    ets:insert(Jobs, {DependentId, [{Queue, Id}|Dependencies], Dependents, Callbacks, Job}),
    Queue2 = queue:filter(fun
    (QueuedId) when QueuedId == DependentId -> false;
    (_) -> true
    end, Queue1),
    {ok, StatusPid} = emq_sup:get_status(local),
    emq_status:put(StatusPid, DependentId, pending),
    {noreply, State#state{ queue = Queue2 }}
  end;

handle_info({remove_dependency, DependentId, {Queue, DependencyId}}, State) ->
  #state { jobs = Jobs, queue = Queue1 } = State,
  
  case ets:lookup(Jobs, DependentId) of
  [] -> 
    {noreply, State};
  [{Id, Dependencies1, Dependents, Callbacks, Job}] ->
    Dependencies2 = lists:delete({Queue, DependencyId}, Dependencies1),
    ets:insert(Jobs, {Id, Dependencies2, Dependents, Callbacks, Job}),

    case Dependencies2 of
    [] ->
      Queue2 = queue:in(Id, Queue1),
      {ok, StatusPid} = emq_sup:get_status(local),
      emq_status:put(StatusPid, DependentId, queued),
      
      {ok, EmqPid} = emq_sup:get_emq(local),
      EmqPid ! kick
      ,
      {noreply, State#state{ queue = Queue2 }};
    _Else ->
      {noreply, State}
    end
  end.



%%% Before stopping the server
terminate(_Reason, State) ->
  #state { jobs = Jobs } = State,
  ets:delete(Jobs),
  ok.


%%% Code Changes
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



%%% Internal API