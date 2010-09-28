-module(cap_config2).
-behaviour(gen_server).



get(Key) ->
  get(node(), Key, undefined).
get(Scope, Key) ->
  get(Scope, Key, undefined).
get(Scope, Key, Default) ->
  gen_server:call(?MODULE, {get, Scope, Key, Default}).

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

-record(ctx, {
  monitors=[],
  callbacks=[],
  global_values=dict:new(),
  node_values=dict:new(),
  local_values=dict:new()
}).

init([]) ->
  {ok, #ctx{}}.

handle_call({get, global, Key, Default}, _From, #ctx{}=Ctx) ->
  case dict:find(Key, Ctx#ctx.global_values) of
  {ok, Value} -> {reply, Value,   Ctx};
  _           -> {reply, Default, Ctx}
  end;
handle_call({get, {node, Node}, Key, Default}, _From, #ctx{}=Ctx) ->
  case dict:find({Node, Key}, Ctx#ctx.node_values) of
  {ok, Value} -> {reply, Value,   Ctx};
  _           -> handle_call({get, global, Key, Default}, _From, Ctx)
  end;
handle_call({get, {Type, Id}, Key, Default}, _From, #ctx{}=Ctx) ->
  case dict:find({Type, Id, Key}, Ctx#ctx.local_values) of
  {ok, Value} -> {reply, Value,   Ctx};
  _1 ->
    case dict:find({Type, Id, "node"}, Ctx#ctx.local_values) of
    {ok, Node} -> handle_call({get, {node, Node}, Key, Default}, _From, Ctx);
    _2         -> {reply, Default, Ctx}
  end;

handle_call({get_all, Key}, _From, #ctx{}=Ctx) ->
  #ctx{ global_values=D1, node_values=D2, local_values=D3 } = Ctx,
  All1 = [],

  All2 = dict:foldl(fun(K, V, Acc) ->
    case K of
    K when K = Key -> [{global, Value}|Acc];
    _              -> Acc
    end
  end, All1, D1),

  All3 = dict:foldl(fun({Node, K}, V, Acc) ->
    case K of
    K when K = Key -> [{{node, Node}, Value}|Acc];
    _              -> Acc
    end
  end, All2, D2),

  All4 = dict:foldl(fun({Type, Id, K}, V, Acc) ->
    case K of
    K when K = Key -> [{{Type, Id}, Value}|Acc];
    _              -> Acc
    end
  end, All3, D3),

  {reply, All3, Ctx};

handle_cast({set, global, Key, Value}, #ctx{}=Ctx) ->
  #ctx{ global_values=D1 } = Ctx,
  D2 = dict:store(Key, Value, D1),
  {norely, Ctx#ctx{ global_values=D2 }};
handle_cast({set, {node, Node}, Key, Value}, #ctx{}=Ctx) ->
  #ctx{ node_values=D1 } = Ctx,
  D2 = dict:store({Node, Key}, Value, D1),
  {norely, Ctx#ctx{ node_values=D2 }};
handle_cast({set, {Type, Id}, Key, Value}, #ctx{}=Ctx) ->
  #ctx{ local_values=D1 } = Ctx,
  D2 = dict:store({Type, Id, Key}, Value, D1),
  {norely, Ctx#ctx{ local_values=D2 }};

handle_cast({unset, global, Key}, #ctx{}=Ctx) ->
  #ctx{ global_values=D1 } = Ctx,
  D2 = dict:erase(Key, D1),
  {norely, Ctx#ctx{ global_values=D2 }};
handle_cast({unset, {node, Node}, Key}, #ctx{}=Ctx) ->
  #ctx{ node_values=D1 } = Ctx,
  D2 = dict:erase({Node, Key}, D1),
  {norely, Ctx#ctx{ node_values=D2 }};
handle_cast({unset, {Type, Id}, Key}, #ctx{}=Ctx) ->
  #ctx{ local_values=D1 } = Ctx,
  D2 = dict:erase({Type, Id, Key}, D1),
  {norely, Ctx#ctx{ local_values=D2 }};
