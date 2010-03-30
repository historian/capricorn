-module(emq).
-behaviour(gen_server).



-export([new/2]).
-export([push/3, push/4, push/5, push/6]).
-export([size/1, jobs/1, status/2, status/3]).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([test/0]).



-record(state, {
  name   :: atom(),
  queue  :: pid(),
  pool   :: pid(),
  status :: pid()
}).
% -type state() :: #state{} .



%%% External API
new(Name, Options) ->
  emq_sup:start_queue(Name, Options).



push(QueueName, Id, Fun) ->
  push(QueueName, Id, [], Fun).

push(QueueName, Id, Dependencies, Fun) ->
  push(QueueName, Id, Dependencies, [], Fun).

push(QueueName, Id, Dependencies, Dependents, Fun) ->
  push(QueueName, Id, Dependencies, Dependents, [], Fun).

push(QueueName, Id, Dependencies, Dependents, Callbacks, Fun) ->
  {ok, Pid} = emq_sup:get_emq(QueueName),
  gen_server:cast(Pid, {push, Id, Dependencies, Dependents, Callbacks, Fun}),
  {ok, {QueueName, Id}}.



size(QueueName) ->
  {ok, Pid} = emq_sup:get_queue(QueueName),
  emq_queue:size(Pid).



jobs(QueueName) ->
  {ok, Pid} = emq_sup:get_status(QueueName),
  emq_status:get_all(Pid).



status(QueueName, Id) ->
  {ok, Pid} = emq_sup:get_emq(QueueName),
  gen_server:call(Pid, {status, Id}).

status(QueueName, Id, Status) ->
  {ok, Pid} = emq_sup:get_emq(QueueName),
  gen_server:cast(Pid, {status, Id, Status}).



%%% Start the server
-spec start_link([{size, pos_integer()}]) -> {ok, pid()} .
start_link(Options) ->
  gen_server:start_link(?MODULE, Options, []).



%%% Initialize the server
init(Options) ->
  Name     = proplists:get_value(name, Options),
  PoolSize = proplists:get_value(size, Options, 3),
  {ok, Pool}   = emq_pool:start_link(Name, PoolSize),
  {ok, Queue}  = emq_queue:start_link(Name),
  {ok, Status} = emq_status:start_link(Name),
  
  emq_sup:setup_process(Pool ,  Name, self(), Queue, Pool, Status),
  emq_sup:setup_process(Queue,  Name, self(), Queue, Pool, Status),
  emq_sup:setup_process(Status, Name, self(), Queue, Pool, Status),
  
  erlang:put('$emq_name',   Name),
  erlang:put('$emq',        self()),
  erlang:put('$emq_pool',   Pool),
  erlang:put('$emq_queue',  Queue),
  erlang:put('$emq_status', Status),
  
  {ok, #state{ pool = Pool, queue = Queue, status = Status, name = Name }}.



%%% Handle call messages
handle_call({'$emq_internal_info'}, _From, State) ->
  {reply, {ok, {
    erlang:get('$emq_name'),
    erlang:get('$emq_pool'),
    erlang:get('$emq_queue'),
    erlang:get('$emq_status')
  }}, State};

handle_call({status, Id}, _From, State) ->
  #state { status = Status } = State,
  {reply, emq_status:get(Status, Id), State}.



%%% Handle cast messages
handle_cast({push, Id, Dependencies, Dependents, Callbacks, Fun}, State) ->
  #state { queue = Queue } = State,
  emq_queue:push(Queue, Id, Dependencies, Dependents, Callbacks, Fun),
  do_try_exec(State),
  {noreply, State};

handle_cast({status, Id, StatusObject}, State) ->
  #state { status = Status } = State,
  emq_status:put(Status, Id, StatusObject),
  {noreply, State}.



%%% Handle generic messages
handle_info(kick, State) ->
  do_try_exec(State),
  {noreply, State};

handle_info({pool_did_job, Id}, State) ->
  #state { queue = Queue } = State,
  emq_queue:commit(Queue, Id),
  do_try_exec(State),
  {noreply, State}.



%%% Before stopping the server
terminate(_Reason, _State) ->
  % stop children
  ok.



%%% Code Changes
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



%%% Internal API
do_try_exec(State) ->
  #state { queue = Queue, pool = Pool } = State,
  {ok, StatusPid} = emq_sup:get_status(local),
  case emq_queue:reserve(Queue) of
  {value, Id, Job, Callbacks} ->
    R = emq_pool:do(Pool, fun({Id1, Job1, Callbacks1, StatusPid1}) ->
      try
        emq_status:put(StatusPid1, Id1, active),
        case Job1 of
        {Mod, Fun, Args} -> erlang:apply(Mod, Fun, Args);
        {Fun, Args}      -> erlang:apply(Fun, Args);
        Fun              -> Fun()
        end
      of
        Result ->
          lists:map(fun(Callback) ->
            Callback(done, Result)
          end, Callbacks1)
      catch
        error:Reason ->
          lists:map(fun(Callback) ->
            Callback(error, Reason)
          end, Callbacks1),
          error_logger:error_report({error, Id1, Reason, erlang:get_stacktrace()});
        throw:Reason ->
          lists:map(fun(Callback) ->
            Callback(throw, Reason)
          end, Callbacks1),
          error_logger:error_report({throw, Id1, Reason});
        exit:Reason ->
          lists:map(fun(Callback) ->
            Callback(exit, Reason)
          end, Callbacks1),
          error_logger:error_report({exit, Id1, Reason})
      after
        emq_status:delete(StatusPid1, Id1)
      end,
      Id1
    end, {Id, Job, Callbacks, StatusPid}),
    case R of
    {error, no_workers} ->
      emq_queue:rollback(Queue, Id);
    ok ->
      ignore
    end;
  {empty} -> ignore
  end.



test() ->
  application:start(emq),
  
  emq:new(hello, [{size, 1}]),
  
  emq:push(hello, hi, fun() ->
    receive after 3000 -> io:format("hi ") end
  end),
  emq:push(hello, simon, [{hello, hi}], fun() ->
    receive after 3000 -> io:format("simon!\n") end
  end),
  
  emq:push(hello, bye, fun() ->
    receive after 3000 -> io:format("bye ") end
  end),
  emq:push(hello, simon, [{hello, bye}], fun() ->
    receive after 3000 -> io:format("simon!\n") end
  end),
  
  % {ok, Id1} = emq:push(hello, make_ref(), fun() ->
  %   receive after 3000 -> io:format("hi ") end
  % end),
  % emq:push(hello, make_ref(), [Id1], fun() ->
  %   receive after 3000 -> io:format("simon!\n") end
  % end),
  % 
  % {ok, Id2} = emq:push(hello, make_ref(), fun() ->
  %   receive after 3000 -> io:format("hi ") end
  % end),
  % emq:push(hello, make_ref(), [Id2], fun() ->
  %   receive after 3000 -> io:format("simon!\n") end
  % end),
  % 
  % {ok, Id3} = emq:push(hello, make_ref(), fun() ->
  %   receive after 3000 -> io:format("hi ") end
  % end),
  % emq:push(hello, make_ref(), [Id3], fun() ->
  %   receive after 3000 -> io:format("simon!\n") end
  % end),
  % 
  % {ok, Id4} = emq:push(hello, make_ref(), fun() ->
  %   receive after 3000 -> io:format("hi ") end
  % end),
  % emq:push(hello, make_ref(), [Id4], fun() ->
  %   receive after 3000 -> io:format("simon!\n") end
  % end),
  % 
  % {ok, Id5} = emq:push(hello, make_ref(), fun() ->
  %   receive after 3000 -> io:format("hi ") end
  % end),
  % emq:push(hello, make_ref(), [Id5], fun() ->
  %   receive after 3000 -> io:format("simon!\n") end
  % end),
  
  ok.