-module(bertrpc_connection).
-behaviour(gen_server).


-export([call/4, call/5, cast/4, info/3]).

-export([start_link/3, start/3, start_link/1, start/1]).
-export([start_link/4, start/4, start_link/2, start/2]).
-export([stop/1]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).


-record(state, {
  pid,
  sock,
  handler,
  state,
  
  type,
  action,
  infos=[],
  mode=term,
  data,
  
  reply_to
}).


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


-spec call(pid(), atom(), atom(), [term()]) -> ok .
cast(Pid, Module, Function, Arguments) ->
  gen_server:call(Pid, {cast, Module, Function, Arguments}).


%%% Start the server
-spec connect({local, atom()} | {global, atom()}, string(), pos_integer()) ->
  {ok, pid()} | {error, term()} .
connect(Name, Host, Port) ->
  gen_server:start(Name, ?MODULE, {connect, Host, Port, undefined, []}, []).

-spec connect_link({local, atom()} | {global, atom()}, string(), pos_integer()) ->
  {ok, pid()} | {error, term()} .
connect_link(Name, Host, Port) ->
  gen_server:start_link(Name, ?MODULE, {connect, Host, Port, undefined, []}, []).

-spec connect({local, atom()} | {global, atom()}, string(), pos_integer(), atom(), term()) ->
  {ok, pid()} | {error, term()} .
connect(Name, Host, Port, Handler, Args) ->
  gen_server:start(Name, ?MODULE, {connect, Host, Port, Handler, Args}, []).

-spec connect_link({local, atom()} | {global, atom()}, string(), pos_integer(), atom(), term()) ->
  {ok, pid()} | {error, term()} .
connect_link(Name, Host, Port, Handler, Args) ->
  gen_server:start_link(Name, ?MODULE, {connect, Host, Port, Handler, Args}, []).


-spec connect(string(), pos_integer()) ->
  {ok, pid()} | {error, term()} .
connect(Host, Port) ->
  gen_server:start(?MODULE, {connect, Host, Port, undefined, []}, []).

-spec connect_link(string(), pos_integer()) ->
  {ok, pid()} | {error, term()} .
connect_link(Host, Port) ->
  gen_server:start_link(?MODULE, {connect, Host, Port, undefined, []}, []).

-spec connect(string(), pos_integer(), atom(), term()) ->
  {ok, pid()} | {error, term()} .
connect(Host, Port, Handler, Args) ->
  gen_server:start(?MODULE, {connect, Host, Port, Handler, Args}, []).

-spec connect_link(string(), pos_integer(), atom(), term()) ->
  {ok, pid()} | {error, term()} .
connect_link(Host, Port, Handler, Args) ->
  gen_server:start_link(?MODULE, {connect, Host, Port, Handler, Args}, []).


-spec listen({local, atom()} | {global, atom()}, pos_integer(), atom(), term()) ->
  {ok, pid()} | {error, term()} .
listen(Name, Port, Handler, Args) ->
  gen_server:start(Name, ?MODULE, {listen, Port, Handler, Args}, []).

-spec listen_link({local, atom()} | {global, atom()}, pos_integer(), atom(), term()) ->
  {ok, pid()} | {error, term()} .
listen_link(Name, Port, Handler, Args) ->
  gen_server:start_link(Name, ?MODULE, {listen, Port, Handler, Args}, []).


-spec listen(pos_integer(), atom(), term()) ->
  {ok, pid()} | {error, term()} .
listen(Port, Handler, Args) ->
  gen_server:start(?MODULE, {listen, Port, Handler, Args}, []).

-spec listen_link(pos_integer(), atom(), term()) ->
  {ok, pid()} | {error, term()} .
listen_link(Port, Handler, Args) ->
  gen_server:start_link(?MODULE, {listen, Port, Handler, Args}, []).


-spec stop(pid()) -> ok .
stop(Pid) ->
  gen_server:cast(Pid, stop).


%%% Initialize the server
init({connect, Host, Port, Handler, Args}) ->
  {ok, Sock} = gen_tcp:connect(Host, Port, [binary, {packet, 4}, {active, true}]),
  if Handler /= undefined ->
    {ok, HandlerState} = Handler:init(Args),
  true ->
    ignore
  end,
  {ok, #state{sock=Sock, handler=Handler, state=HandlerState}};
  
init({listen, Port, Handler, Args}) ->
  {ok, LSock} = gen_tcp:listen(Port, [binary, {packet, 4}, {active, true}]),
  {ok, #state{sock=Sock, handler=Handler, state=HandlerState}}.
% {tcp, Socket, Data}
% {tcp_closed, Socket}
% {tcp_error, Socket, Reason}


%%% Handle call messages
handle_call({call, Module, Function, Arguments}, From, State) ->
  #state{ sock = Sock } = State,
  
  gen_tcp:send(Sock, bert:encode({call, Module, Function, Arguments})),
  gen_server:cast(self(), {send_stream}),
  
  {reply, ok, State#state{
    reply_to = From
  }}.

handle_call({cast, Module, Function, Arguments}, From, State) ->
  #state{ sock = Sock } = State,
  
  gen_tcp:send(Sock, bert:encode({cast, Module, Function, Arguments})),
  gen_server:cast(self(), {send_stream}),
  
  {reply, ok, State#state{
    reply_to = From
  }}.


%%% Handle cast messages
handle_cast({info, stream, [Data]}, State) when is_binary(Data) ->
  #state{ sock = Sock } = State,
  
  gen_tcp:send(Sock, bert:encode({info, stream, []})),
  
  {noreply, State#state { data = Data }};

handle_cast({info, stream, [Path]}, State) when is_list(Path) ->
  #state{ sock = Sock } = State,
  
  {ok, Data} = file:read_file(Path),
  gen_tcp:send(Sock, bert:encode({info, stream, []})),
  
  {noreply, State#state { data = Data }};

handle_cast({info, Command, Options}, State) ->
  #state{ sock = Sock } = State,
  
  gen_tcp:send(Sock, bert:encode({info, Command, Options})),
  
  {noreply, State};

handle_cast({send_stream}, State) ->
  #state{ sock = Sock, data=Data } = State,
  
  if is_binary(Data) ->
    gen_tcp:send(Sock, Data);
  true ->
    ignore
  end,
  
  {noreply, State};

handle_cast({recv, Data}, #state{mode=term}=State) ->
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
    do_handle_noreply(State);
  end,
  {noreply, State};

handle_cast({recv, <<"">>}, #state{mode=data}=State1) ->
  State2 = do_handle_action(State1),
  {noreply, State2};
  
handle_cast({recv, Data1}, #state{mode=data}=State1) ->
  #state{ data = Data2 } = State1,
  State2 = State#state{
    data = <<Data2/binary, Data1/binary>>
  },
  {noreply, State2};

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Msg, State) ->
  {noreply, State}.


%%% Handle generic messages
handle_info(_Info, State) ->
  {noreply, State}.


%%% Before stopping the server
terminate(_Reason, State) ->
  #state{ sock = Sock } = State,
  gen_tcp:close(Sock),
  ok.


%%% Code Changes
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%% Internal API

do_loop(Owner, Sock) ->
  case gen_tcp:recv(Sock, 0) of
  {error, closed} ->
    gen_tcp:close(Sock),
    stop(Owner),
    exit(normal);
  {ok, Data} ->
    gen_server:cast(Owner, {rscv, Data})
  end


do_handle_info(stream, [], State) ->
  State#state{ data=<<"">> };

do_handle_info(Command, Options, #state{request=Req}=State) ->
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
  cast -> do_handle_cast(Module, Function, Arguments, State);
  end.


do_handle_call(Module, Function, Arguments, State) ->
  #state{
    sock=Sock,
    handler=Handler,
    state=HandlerState,
    
    action={Module, Function, Arguments},
    infos=Infos,
    data=Data
  } = State,
  
  case Handler:handle_call(Module, Function, Arguments, {Infos, Data}, HandlerState) of
  {reply, Reply, NewHandlerState} ->
    gen_tcp:send(Sock, bert:encode({reply, Reply})),
    do_reset_state(State, NewHandlerState);
  {error, Reason, NewHandlerState} ->
    gen_tcp:send(Sock, bert:encode({error, Reason})),
    do_reset_state(State, NewHandlerState)
  end.


do_handle_cast(Module, Function, Arguments, State) ->
  #state{
    sock=Sock,
    handler=Handler,
    state=HandlerState,
    
    action={Module, Function, Arguments},
    infos=Infos,
    data=Data
  } = State,
  
  gen_tcp:send(Sock, bert:encode({noreply})),
  
  case Handler:handle_cast(Module, Function, Arguments, {Infos, Data}, HandlerState) of
  {noreply, NewHandlerState} ->
    do_reset_state(State, NewHandlerState);
  {error, Reason, NewHandlerState} ->
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

