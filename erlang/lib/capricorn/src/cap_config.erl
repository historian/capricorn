-module(cap_config).
-behaviour(gen_server).

-export([
  start_link/0,
  get/1, get/2, get/3,
  get_all/0, get_all/1,
  set/2, set/3,
  unset/1, unset/2,
  sync/0
]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

start_link() ->
  case whereis(?MODULE) of
  undefined -> gen_server:start_link({local, ?MODULE}, ?MODULE, [], []);
  Pid -> {ok, Pid}
  end.

get(Key) ->
  get({node, node()}, Key, undefined).
get(Scope, Key) ->
  get(Scope, Key, undefined).
get(Scope, Key, Default) ->
  gen_server:call(?MODULE, {get, Scope, Key, Default}).

get_all() ->
  gen_server:call(?MODULE, {get_all}).
get_all(Key) ->
  gen_server:call(?MODULE, {get_all, Key}).

set(Key, Value) ->
  set(node(), Key, Value).
set(Scope, Key, Value) ->
  gen_server:cast(?MODULE, {set, Scope, Key, Value}).

unset(Key) ->
  set(node(), Key).
unset(Scope, Key) ->
  gen_server:cast(?MODULE, {set, Scope, Key}).

sync() ->
  gen_server:cast(?MODULE, {sync}).

-record(ctx, {
  monitors=[],
  callbacks=[],
  tab
}).

init([]) ->
  {ok, [[Base]]} = init:get_argument(cap_etc),
  Path = filename:join(Base, 'config.db'),
  {ok, Tab} = dets:open_file(cap_config_table, [
    {file, Path}
  ]),

  pg2:create(cap_config_pool),
  pg2:join(cap_config_pool, self()),

  {ok, #ctx{tab = Tab}}.

handle_call({get, global, Key, Default}, _From, #ctx{}=Ctx) ->
  case dets:lookup(Ctx#ctx.tab, {global, Key}) of
  [{_, Value, _}] -> {reply, Value,   Ctx};
  _               -> {reply, Default, Ctx}
  end;
handle_call({get, {node, Node}, Key, Default}, _From, #ctx{}=Ctx) ->
  case dets:lookup(Ctx#ctx.tab, {node, Node, Key}) of
  [{_, Value, _}] -> {reply, Value,   Ctx};
  _               -> handle_call({get, global, Key, Default}, _From, Ctx)
  end;
handle_call({get, {Type, Id}, Key, Default}, _From, #ctx{}=Ctx) ->
  case dets:lookup(Ctx#ctx.tab, {local, Type, Id, Key}) of
  [{_, Value, _}] -> {reply, Value,   Ctx};
  _1 ->
    case dets:lookup(Ctx#ctx.tab, {local, Type, Id, "node"}) of
    [{_, Node, _}] -> handle_call({get, {node, Node}, Key, Default}, _From, Ctx);
    _2             -> handle_call({get, global, Key, Default}, _From, Ctx)
    end
  end;

handle_call({get_all, Key}, _From, #ctx{}=Ctx) ->
  All1 = [],

  All2 = dets:foldr(fun
  ({{global, K}, V, _}, Acc) when K =:= Key ->
    [{global, V}|Acc];
  (_, Acc) ->
    Acc
  end, All1, Ctx#ctx.tab),

  All3 = dets:foldr(fun
  ({{node, Node, K}, V, _}, Acc) when K =:= Key ->
    [{{node, Node}, V}|Acc];
  (_, Acc) ->
    Acc
  end, All2, Ctx#ctx.tab),

  All4 = dets:foldr(fun
  ({{local, Type, Id, K}, V, _}, Acc) when K =:= Key ->
    [{{local, Type, Id}, V}|Acc];
  (_, Acc) ->
    Acc
  end, All3, Ctx#ctx.tab),

  {reply, All4, Ctx};

handle_call({get_all}, _From, #ctx{}=Ctx) ->
  Objects = dets:foldr(fun
  (Object, Acc) ->
    [Object|Acc]
  end, [], Ctx#ctx.tab),
  {reply, Objects, Ctx}.

handle_cast({set, global, Key, Value}, #ctx{}=Ctx) ->
  dets:insert(Ctx#ctx.tab, {{global, Key}, Value, erlang:now()}),
  {noreply, Ctx};
handle_cast({set, {node, Node}, Key, Value}, #ctx{}=Ctx) ->
  dets:insert(Ctx#ctx.tab, {{node, Node, Key}, Value, erlang:now()}),
  {noreply, Ctx};
handle_cast({set, {Type, Id}, Key, Value}, #ctx{}=Ctx) ->
  dets:insert(Ctx#ctx.tab, {{local, Type, Id, Key}, Value, erlang:now()}),
  {noreply, Ctx};

handle_cast({unset, global, Key}, #ctx{}=Ctx) ->
  dets:delete(Ctx#ctx.tab, {global, Key}),
  {noreply, Ctx};
handle_cast({unset, {node, Node}, Key}, #ctx{}=Ctx) ->
  dets:delete(Ctx#ctx.tab, {node, Node, Key}),
  {noreply, Ctx};
handle_cast({unset, {Type, Id}, Key}, #ctx{}=Ctx) ->
  dets:delete(Ctx#ctx.tab, {local, Type, Id, Key}),
  {noreply, Ctx};

handle_cast({sync}, #ctx{}=Ctx) ->
  handle_cast({sync, pg2:get_members(cap_config_pool)}, Ctx);

handle_cast({sync, [Peer |Other]}, #ctx{}=Ctx) when Peer =:= self() ->
  handle_cast({sync, Other}, Ctx);

handle_cast({sync, [Peer |Other]}, #ctx{}=Ctx) ->
  Objects1 = gen_server:call(Peer, {get_all}),

  Objects2 = lists:foldl(fun
  ({Key, _, Date1}=Object1, Acc) ->

    case dets:lookup(Ctx#ctx.tab, Key) of
    [{_,_,Date2}=Object2] when Date1 =< Date2 ->
      [Object2|Acc];
    [{_,_,Date2}] when Date1 > Date2 ->
      [Object1|Acc];
    _ ->
      [Object1|Acc]
    end

  end, [], Objects1),

  dets:insert(Ctx#ctx.tab, Objects2),

  handle_cast({sync, Other}, Ctx);

handle_cast({sync, []}, #ctx{}=Ctx) ->
  {noreply, Ctx}.


handle_info(_, State) ->
  {noreply, State}.


terminate(_Reason, #ctx{}=Ctx) ->
  pg2:leave(cap_config_pool, self()),
  dets:close(Ctx#ctx.tab),
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
