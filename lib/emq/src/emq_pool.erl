-module(emq_pool).
-behaviour(gen_server).



-export([size/1, size/2, active/1, idle/1, do/3]).

-export([start_link/2]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([do_init_worker/2]).



-record(state, {
  name    :: atom(),
  workers :: [pid()],
  waiters :: [pid()],
  size    :: pos_integer(),
  owner   :: pid()
}).
-type state() :: #state{} .



%%% External API
size(Pid) ->
  gen_server:call(Pid, {size}).



size(Pid, Size) ->
  gen_server:call(Pid, {size, Size}).



active(Pid) ->
  gen_server:call(Pid, {active}).



idle(Pid) ->
  gen_server:call(Pid, {idle}).



do(Pid, Fun, Ctx) ->
  gen_server:call(Pid, {do, Fun, Ctx}).



%%% Start the server
-spec start_link(atom(), pos_integer()) -> {ok, pid()} .
start_link(Name, Size) ->
  gen_server:start_link(?MODULE, {Name, Size, self()}, []).



%%% Initialize the server
-spec init({atom(), pos_integer(), pid()}) -> {ok, state()} .
init({Name, Size, Owner}) ->
  {ok, #state{ waiters=[], workers=[], size=Size, owner=Owner, name=Name }}.



%%% Handle call messages
handle_call({setup_process, Name, Emq, Queue, Pool, Status}, _From, State) ->
  erlang:put('$emq_name',   Name),
  erlang:put('$emq',        Emq),
  erlang:put('$emq_pool',   Pool),
  erlang:put('$emq_queue',  Queue),
  erlang:put('$emq_status', Status),
  {reply, ok, State};

handle_call({size, Size}, _From, State) ->
  {reply, ok, State#state{ size=Size }};

handle_call({size}, _From, State) ->
  {reply, State#state.size, State};

handle_call({active}, _From, State) ->
  {reply, erlang:length(State#state.workers), State};

handle_call({idle}, _From, State) ->
  {reply, erlang:length(State#state.waiters), State};

handle_call({do, Fun, Ctx}, _From, State) ->
  case do_get_worker(State) of
  {State1, Worker} ->
    Worker ! {exec, Fun, Ctx},
    {reply, ok, State1};
  {State1} ->
    {reply, {error, no_workers}, State1}
  end.



%%% Handle cast messages
handle_cast(_Msg, State) ->
  {noreply, State}.


%%% Handle generic messages
handle_info({done, Worker, Ctx}, State) ->
  #state { waiters = Waiters, workers = Workers, owner = Owner } = State,
  Owner ! {pool_did_job, Ctx},
  {noreply, State#state{
    waiters = [Worker|Waiters],
    workers = lists:delete(Worker, Workers)
  }};

handle_info({'DOWN', _, process, Worker, Reason}, State) ->
  error_logger:error_msg("Worker Exited unexpectedly~n", []),
  error_logger:error_report(Reason),
  #state { waiters = Waiters, workers = Workers } = State,
  {noreply, State#state{
    waiters = lists:delete(Worker, Waiters),
    workers = lists:delete(Worker, Workers)
  }}.


%%% Before stopping the server
terminate(_Reason, _State) ->
  ok.


%%% Code Changes
code_change(_OldVsn, State, _Extra) ->
  [Pid1 ! stop || Pid1 <- State#state.workers],
  [Pid2 ! stop || Pid2 <- State#state.waiters],
  {ok, State}.


%%% Internal API
-spec do_get_worker(state()) -> {state(), pid()} | {state()} .
do_get_worker(State) ->
  #state { size = Size, workers = Workers, waiters = Waiters, owner = Owner } = State,
  if
  length(Waiters) > 0 ->
    [Pid | Waiters2] = Waiters,
    Workers2 = [Pid | Workers],
    {State#state { workers=Workers2, waiters=Waiters2 }, Pid};
  length(Workers) < Size ->
    Pid = spawn(?MODULE, do_init_worker, [self(), Owner]),
    erlang:monitor(process, Pid),
    Pid ! go,
    {State#state { workers=[Pid | Workers] }, Pid};
  true ->
    {State}
  end.

do_init_worker(Pool, Owner) ->
  erlang:monitor(process, Pool),
  receive
  go ->
    do_loop_worker(Pool, Owner);
  stop ->
    erlang:exit(normal);
  {'DOWN', _, process, Pool, _} ->
    erlang:exit(queue_down)
  after 300000 ->
    erlang:exit(timeout)
  end.

do_loop_worker(Pool, Owner) ->
  receive
  stop ->
    erlang:exit(normal);
  {'DOWN', _, process, Pool, _} ->
    erlang:exit(queue_down);
  {exec, Fun, Ctx1} ->
    Ctx2 = erlang:apply(Fun, [Ctx1]),
    Pool ! {done, self(), Ctx2},
    do_loop_worker(Pool, Owner)
  after 300000 ->
    erlang:exit(timeout)
  end.
