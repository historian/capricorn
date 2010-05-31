-module(emq_sup).
-behaviour(supervisor).

-export([start_link/0, stop/0]).
-export([start_queue/2, setup_process/6, get_emq/1, get_queue/1, get_pool/1, get_status/1]).
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

stop() ->
  erlang:exit(whereis(?MODULE), normal).

start_queue(Name, Options) ->
  Result = supervisor:start_child(?MODULE, {
    Name,
    {emq, start_link, [[{name, Name}| Options]]},
    permanent,
    5000,
    worker,
    [emq]
  }),
  case Result of
  {ok, _Pid, _Info} ->
    {ok, Name};
  {ok, _Pid} ->
    {ok, Name};
  {error, Reason} ->
    {error, Reason}
  end.



setup_process(Pid, Name, Emq, Queue, Pool, Status) ->
  gen_server:call(Pid, {setup_process, Name, Emq, Queue, Pool, Status}).



get_emq(local) ->
  {ok, erlang:get('$emq')};

get_emq({remote, Name}) ->
  Children = supervisor:which_children(?MODULE),
  lists:foldl(fun
  ({ChildName, Pid, worker, [emq]}, _) when Name == ChildName ->
    {ok, Pid};
  (_, Acc) ->
    Acc
  end, {error, not_found}, Children);

get_emq(Name) ->
  case erlang:get('$emq_name') of
  MyName when MyName == Name ->
    get_emq(local);
  _Else ->
    get_emq({remote, Name})
  end.



get_queue(local) ->
  {ok, erlang:get('$emq_queue')};

get_queue({remote, Name}) ->
  case get_emq({remote, Name}) of
  {ok, Pid} ->
    case gen_server:call(Pid, {'$emq_internal_info'}) of
    {ok, {Name, _Pool, Queue, _Status}} -> {ok, Queue};
    {error, Reason} -> {error, Reason}
    end;
  {error, Reason} -> {error, Reason}
  end;

get_queue(Name) ->
  case erlang:get('$emq_name') of
  MyName when MyName == Name ->
    get_queue(local);
  _Else ->
    get_queue({remote, Name})
  end.



get_pool(local) ->
  {ok, erlang:get('$emq_pool')};

get_pool({remote, Name}) ->
  case get_emq({remote, Name}) of
  {ok, Pid} ->
    case gen_server:call(Pid, {'$emq_internal_info'}) of
    {ok, {Name, Pool, _Queue, _Status}} -> {ok, Pool};
    {error, Reason} -> {error, Reason}
    end;
  {error, Reason} -> {error, Reason}
  end;

get_pool(Name) ->
  case erlang:get('$emq_name') of
  MyName when MyName == Name ->
    get_pool(local);
  _Else ->
    get_pool({remote, Name})
  end.



get_status(local) ->
  {ok, erlang:get('$emq_status')};

get_status({remote, Name}) ->
  case get_emq({remote, Name}) of
  {ok, Pid} ->
    case gen_server:call(Pid, {'$emq_internal_info'}) of
    {ok, {Name, _Pool, _Queue, Status}} -> {ok, Status};
    {error, Reason} -> {error, Reason}
    end;
  {error, Reason} -> {error, Reason}
  end;

get_status(Name) ->
  case erlang:get('$emq_name') of
  MyName when MyName == Name ->
    get_status(local);
  _Else ->
    get_status({remote, Name})
  end.



init([]) ->
  {ok, {{one_for_one, 10, 3600},[
  ]}}.
