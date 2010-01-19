-module(gcd_srv).
-behaviour(gen_server).
-include("gcd.hrl").

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-define(L(State), State#state.local).
-define(R(State), State#state.remote).
-define(N(State), State#state.nodes).
-define(M(State), State#state.monitors).


%%% Start the server
start_link() ->
  gen_server:start_link({local, gcd_srv}, ?MODULE, [], []).


%%% Initialize the server
init([]) ->
  net_kernel:monitor_nodes(true),
  Local    = ets:new(gcd_srv_local,    [set,private,{keypos,2}]),
  Remote   = ets:new(gcd_srv_remote,   [set,private,{keypos,4}]),
  Nodes    = ets:new(gcd_srv_nodes,    [set,private,{keypos,2}]),
  Monitors = ets:new(gcd_srv_monitors, [set,private,{keypos,2}]),
  {ok, #state{local=Local,remote=Remote,nodes=Nodes,monitors=Monitors}}.


%%% Handle call messages
handle_call({foldl, Fun, Acc1}, _From, State) ->
  Acc2 = ets:foldl(Fun, Acc1, ?L(State)),
  Acc3 = ets:foldl(Fun, Acc2, ?R(State)),
  {reply, Acc3, State};

handle_call({match_object, Pattern}, _From, State) ->
  Services1 = ets:match_object(?L(State), Pattern),
  Services2 = ets:match_object(?R(State), Pattern),
  {reply, Services1 ++ Services2, State};

handle_call({register, Pid, Name, Node, Location, Description}, _From, State) ->
  Service = #service{node=Node, location=Location, pid=Pid, name=Name, description=Description},
  case ets:insert_new(?L(State), Service) of
  true ->
    erlang:monitor(process, Pid),
    gen_server:cast(self(), {serviceup, Service}),
    {reply, ok, State};
  false ->
    {reply, {error, already_registerd}, State}
  end;

handle_call({unregister, Pid}, _From, State) ->
  case ets:lookup(?L(State), Pid) of
  [Service] ->
    gen_server:cast(self(), {servicedown, Service}),
    ets:delete(?L(State), Pid);
  _ -> ok
  end,
  {reply, ok, State};

handle_call({monitor, Pid, Pattern}, _From, State) ->
  Monitor = #monitor{ref=erlang:make_ref(), pid=Pid, pattern=Pattern},
  case ets:insert_new(?M(State), Monitor) of
  true ->
    erlang:monitor(process, Pid),
    {reply, Monitor#monitor.ref, State};
  false ->
    {reply, {error, already_registerd}, State}
  end;

handle_call({demonitor, Ref}, _From, State) ->
  ets:delete(?M(State), Ref),
  {reply, ok, State};

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


%%% Handle cast messages
handle_cast({info, Node, Nodes, Services}, State) ->
  ets:insert(?N(State), #node{name=Node}),
  ets:insert(?R(State), Services),
  [net_adm:ping(Node1) || Node1 <- Nodes],
  [gen_server:cast(self(), {serviceup, Service}) || Service <- Services],
  {noreply, State};

handle_cast({serviceup, Service}, State) ->
  case Service#service.pid of
  remote ->
    ets:insert(?R(State), Service);
  _Pid   -> 
    ets:insert(?L(State), Service),
    ets:foldl(fun(#node{name=Node}, _) ->
      gen_server:cast({gcd_srv, Node}, {serviceup, Service#service{pid=remote}})
    end, [], ?N(State))
  end,
  
  ets:foldl(fun(#monitor{pattern=Pattern,pid=Pid,ref=Ref}, _) ->
    case ets:test_ms(Service, [{Pattern,[],[true]}]) of
    {ok, true} ->
      Pid ! {serviceup, Ref, Service};
    _Else -> ignore
    end
  end, [], ?M(State)),
  
  {noreply, State};

handle_cast({servicedown, Service}, State) ->
  case Service#service.pid of
  remote ->
    ets:delete(?R(State), Service#service.location);
  _Pid   ->
    ets:delete(?L(State), Service#service.pid),
    ets:foldl(fun(#node{name=Node}, _) ->
      gen_server:cast({gcd_srv, Node}, {servicedown, Service#service{pid=remote}})
    end, [], ?N(State))
  end,
  
  ets:foldl(fun(#monitor{pattern=Pattern,pid=Pid,ref=Ref}, _) ->
    case ets:test_ms(Service, [{Pattern,[],[true]}]) of
    {ok, true} ->
      Pid ! {servicedown, Ref, Service};
    _Else -> ignore
    end
  end, [], ?M(State)),
  
  {noreply, State};

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Msg, State) ->
  {noreply, State}.


%%% Handle generic messages
handle_info({'DOWN', _Ref, process, Pid, _Reason}, State) ->
  case ets:lookup(?L(State), Pid) of
  [Service] ->
    gen_server:cast(self(), {servicedown, Service}),
    ets:delete(?L(State), Pid);
  _ -> ok
  end,
  Monitors = ets:foldl(fun
  (#monitor{ref=Ref,pid=Pid1}, Acc) when Pid1 == Pid ->
    [Ref|Acc];
  (_Else, Acc) -> Acc
  end, [], ?M(State)),
  [ets:delete(?M(State), Ref) || Ref <- Monitors],
  {noreply, State};

handle_info({nodeup, Node}, State) ->
  gen_server:cast({gcd_srv, Node}, {info, node(),
    ets:foldl(fun(#node{name=N}, Acc) -> [N|Acc]                     end, [], ?N(State)),
    ets:foldl(fun(#service{}=S, Acc)  -> [S#service{pid=remote}|Acc] end, [], ?L(State))
  }),
  
  {noreply, State};

handle_info({nodedown, Node}, State) ->
  ets:delete(?N(State), Node),
  
  Keys = ets:foldl(fun
  (#service{node=Node1,location=L}=Service, Acc) when Node1 == Node->
    gen_server:cast(self(), {servicedown, Service}),
    [L|Acc];
  (_Else, Acc) -> Acc
  end, [], ?R(State)),
  [ets:delete(?R(State), Key) || Key <- Keys],
  
  {noreply, State};

handle_info(_Info, State) ->
  {noreply, State}.

%%% Before stopping the server
terminate(_Reason, State) ->
  ets:delete(?L(State)),
  ets:delete(?R(State)),
  ets:delete(?N(State)),
  ets:delete(?M(State)),
  ok.

%%% Code Changes
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
