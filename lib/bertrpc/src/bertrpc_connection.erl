-module(bertrpc_connection).
-behaviour(tcp_server).


-export([call/4, call/5, cast/4, info/3]).

-export([listen_link/4, listen_link/3, listen/4, listen/3]).
-export([connect_link/3, connect_link/2, connect/3, connect/2]).

-export([behaviour_info/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_data/2,
         handle_info/2, terminate/2, code_change/3]).


-record(state, {
  callback,
  state,
  
  type,
  action,
  infos=[],
  mode=term,
  data,
  
  reply_to
}).
-define(SOCK_OPTS, [binary, {packet, 4}]).


%%% Behaviour API
behaviour_info(callbacks) ->
  [{init,1}, {handle_call, 3}, {handle_cast, 2}, {handle_info, 2}, {terminate,2}, {code_change,3}];
behaviour_info(_) ->
  undefined.


%%% Start the server
listen_link(Name, Callback, Args, Port) ->
  tcp_server:listen_link(Name, ?MODULE, {Callback, Args}, Port, ?SOCK_OPTS).

listen_link(Callback, Args, Port) ->
  tcp_server:listen_link(?MODULE, {Callback, Args}, Port, ?SOCK_OPTS).


listen(Name, Callback, Args, Port) ->
  tcp_server:listen(Name, ?MODULE, {Callback, Args}, Port, ?SOCK_OPTS).

listen(Callback, Args, Port) ->
  tcp_server:listen(?MODULE, {Callback, Args}, Port, ?SOCK_OPTS).


%%% Start the client
connect_link(Name, Host, Port) ->
  tcp_server:connect_link(Name, ?MODULE, {}, Host, Port, ?SOCK_OPTS).

connect_link(Host, Port) ->
  tcp_server:connect_link(?MODULE, {}, Host, Port, ?SOCK_OPTS).


connect(Name, Host, Port) ->
  tcp_server:connect(Name, ?MODULE, {}, Host, Port, ?SOCK_OPTS).

connect(Host, Port) ->
  tcp_server:connect(?MODULE, {}, Host, Port, ?SOCK_OPTS).


%%% External API
-spec info(pid(), atom(), [term()]) -> ok .
info(Pid, Command, Options) ->
  gen_server:cast(Pid, {info, Command, Options}).


-spec call(pid(), atom(), atom(), [term()]) -> term() | {error, term()} .
call(Pid, Module, Function, Arguments) ->
  gen_server:call(Pid, {call, Module, Function, Arguments}).


-spec call(pid(), atom(), atom(), [term()], pos_integer()) -> term() | {error, term()} .
call(Pid, Module, Function, Arguments, Timeout) ->
  gen_server:call(Pid, {call, Module, Function, Arguments}, Timeout).


-spec cast(pid(), atom(), atom(), [term()]) -> ok .
cast(Pid, Module, Function, Arguments) ->
  gen_server:call(Pid, {cast, Module, Function, Arguments}).


%%% Initialize the server
init({Callback, Args}) ->
  {ok, SubState} = Callback:init(Args),
  {ok, #state{callback=Callback, state=SubState}};
  
init({}) ->
  {ok, #state{}}.


%%% Handle call messages
handle_call({call, Module, Function, Arguments}, From, State) ->
  
  tcp_server:send(self(), bert:encode({call, Module, Function, Arguments})),
  tcp_server:cast(self(), {send_stream}),
  
  {noreply, State#state{
    reply_to = From
  }};

handle_call({cast, Module, Function, Arguments}, From, State) ->
  
  tcp_server:send(self(), bert:encode({cast, Module, Function, Arguments})),
  tcp_server:cast(self(), {send_stream}),
  
  {noreply, State#state{
    reply_to = From
  }}.


%%% Handle cast messages
handle_cast({info, stream, [Data]}, State) when is_binary(Data) ->
  
  tcp_server:send(self(), bert:encode({info, stream, []})),
  
  {noreply, State#state { data = Data }};

handle_cast({info, stream, [Path]}, State) when is_list(Path) ->
  
  {ok, Data} = file:read_file(Path),
  tcp_server:send(self(), bert:encode({info, stream, []})),
  
  {noreply, State#state { data = Data }};

handle_cast({info, Command, Options}, State) ->
  
  tcp_server:send(self(), bert:encode({info, Command, Options})),
  
  {noreply, State};

handle_cast({send_stream}, State) ->
  #state{ data=Data } = State,
  
  if is_binary(Data) ->
    tcp_server:send(self(), Data);
  true ->
    ignore
  end,
  
  {noreply, State};

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Msg, State) ->
  {noreply, State}.


%%% Handle generic messages
handle_info(_Info, State) ->
  {noreply, State}.


%%% Handle data messages
handle_data(Data, #state{mode=term}=State) ->
  State1 = 
  case bert:decode(Data) of
  {call, Module, Function, Arguments} -> 
    do_handle_action(call, Module, Function, Arguments, State);
  {cast, Module, Function, Arguments} -> 
    do_handle_action(cast, Module, Function, Arguments, State);
  {info, Command, Options} -> 
    do_handle_info(Command, Options, State);
  {reply, Result} -> 
    do_handle_reply(Result, State);
  {error, Reason} -> 
    do_handle_error(Reason, State);
  {noreply} -> 
    do_handle_noreply(State)
  end,
  {noreply, State1};

handle_data(<<"">>, #state{mode=data}=State1) ->
  State2 = do_handle_action(State1),
  {noreply, State2};
  
handle_data(Data1, #state{mode=data}=State1) ->
  #state{ data = Data2 } = State1,
  State2 = State1#state{
    data = <<Data2/binary, Data1/binary>>
  },
  {noreply, State2};


%%% Before stopping the server
terminate(_Reason, State) ->
  ok.


%%% Code Changes
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%% Internal API
do_handle_info(stream, [], State) ->
  State#state{ data = <<"">> };

do_handle_info(Command, Options, #state{}=State) ->
  #state{ infos = Infos } = State,
  State#state{
    infos = [{Command, Options}|Infos]
  }.


do_handle_action(Type, Module, Function, Arguments, State) ->
  #state{data=Data} = State,
  if is_binary(Data) ->
    State#state{
      type=Type,
      action={Module, Function, Arguments},
      mode=data
    };
  true ->
    do_handle_action(State#state{
      type=Type,
      action={Module, Function, Arguments}
    })
  end.


do_handle_action(State) ->
  #state{
    type=Type,
    action={Module, Function, Arguments}
  } = State,
  
  case Type of
  call -> do_handle_call(Module, Function, Arguments, State);
  cast -> do_handle_cast(Module, Function, Arguments, State)
  end.


do_handle_call(Module, Function, Arguments, State) ->
  #state{
    sock=Sock,
    callback=Handler,
    state=HandlerState,
    
    action={Module, Function, Arguments},
    infos=Infos,
    data=Data
  } = State,
  
  case Handler:handle_call(Module, Function, Arguments, {Infos, Data}, HandlerState) of
  {reply, Reply, NewHandlerState} ->
    tcp_server:send(self(), bert:encode({reply, Reply})),
    do_reset_state(State, NewHandlerState);
  {error, Reason, NewHandlerState} ->
    tcp_server:send(self(), bert:encode({error, Reason})),
    do_reset_state(State, NewHandlerState)
  end.


do_handle_cast(Module, Function, Arguments, State) ->
  #state{
    sock=Sock,
    callback=Handler,
    state=HandlerState,
    
    action={Module, Function, Arguments},
    infos=Infos,
    data=Data
  } = State,
  
  tcp_server:send(self(), bert:encode({noreply})),
  
  case Handler:handle_cast(Module, Function, Arguments, {Infos, Data}, HandlerState) of
  {noreply, NewHandlerState} ->
    do_reset_state(State, NewHandlerState);
  {error, _Reason, NewHandlerState} ->
    %% log error
    do_reset_state(State, NewHandlerState)
  end.


do_reset_state(State) ->
  State#state{action=undefined, infos=[], data=undefined, mode=term}.


do_reset_state(State, NewHandlerState) ->
  do_reset_state(State#state{state=NewHandlerState}).


do_handle_reply(Result, State) ->
  #state{ reply_to = Client } = State,
  gen_server:reply(Client, Result),
  do_reset_state(State).


do_handle_error(Reason, State) ->
  #state{ reply_to = Client } = State,
  gen_server:reply(Client, {error, Reason}),
  do_reset_state(State).


do_handle_noreply(State) ->
  #state{ reply_to = Client } = State,
  gen_server:reply(Client, ok),
  do_reset_state(State).

