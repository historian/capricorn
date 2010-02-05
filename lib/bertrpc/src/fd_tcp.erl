-module(fd_tcp).
-behaviour(gen_server).


-export([listen_link/5, listen_link/4, listen/5, listen/4]).
-export([connect_link/6, connect_link/5, connect/6, connect/5]).
-export([connect_link/4, connect_link/3, connect/4, connect/3]).
-export([send/1, recv/1, recv/2, call/2, call/3, multi_call/2, multi_call/3, multi_call/4, cast/2, abcast/2, abcast/3, reply/2]).
-export([close/1]).

-export([behaviour_info/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).


-record(server, {
  sock, pid,
  callback, args,
  port, options
}).

-record(accept, {
  sock, pid,
  callback, state
}).

-record(client, {
  sock,
  callback, state
}).


%%% Behaviour API
behaviour_info(callbacks) ->
  [{init,1}, {handle_call, 3}, {handle_cast, 2}, {handle_data, 2}, {handle_info, 2}, {terminate,2}, {code_change,3}];
behaviour_info(_) ->
  undefined.


%%% Start the server
listen_link(Name, Callback, Args, Port, Options) ->
  gen_server:start_link(Name, ?MODULE, {listen, Callback, Args, Port, Options}, []).

listen_link(Callback, Args, Port, Options) ->
  gen_server:start_link(?MODULE, {listen, Callback, Args, Port, Options}, []).


listen(Name, Callback, Args, Port, Options) ->
  gen_server:start(Name, ?MODULE, {listen, Callback, Args, Port, Options}, []).

listen(Callback, Args, Port, Options) ->
  gen_server:start(?MODULE, {listen, Callback, Args, Port, Options}, []).


%%% Start the client
connect_link(Name, Callback, Args, Host, Port, Options) ->
  gen_server:start_link(Name, ?MODULE, {connect, Callback, Args, Host, Port, Options}, []).

connect_link(Callback, Args, Host, Port, Options) ->
  gen_server:start_link(?MODULE, {connect, Callback, Args, Host, Port, Options}, []).


connect(Name, Callback, Args, Host, Port, Options) ->
  gen_server:start(Name, ?MODULE, {connect, Callback, Args, Host, Port, Options}, []).

connect(Callback, Args, Host, Port, Options) ->
  gen_server:start(?MODULE, {connect, Callback, Args, Host, Port, Options}, []).


connect_link(Name, Host, Port, Options) ->
  connect_link(Name, undefined, [], Host, Port, Options).

connect_link(Host, Port, Options) ->
  connect_link(undefined, [], Host, Port, Options).


connect(Name, Host, Port, Options) ->
  connect(Name, undefined, [], Host, Port, Options).

connect(Host, Port, Options) ->
  connect(undefined, [], Host, Port, Options).


%%% Stop the server
close(Pid) ->
  gen_server:cast(Pid, fd_tcp_stop).


%%% Send data over the connection
send(Data) ->
  Sock = get(fd_tcp_sock),
  gen_tcp:send(Sock, Data).


recv(Length) ->
  Sock = get(fd_tcp_sock),
  gen_tcp:recv(Sock, Length).

recv(Length, Timeout) ->
  Sock = get(fd_tcp_sock),
  gen_tcp:recv(Sock, Length, Timeout).


call(Pid, Msg) ->
  gen_server:call(Pid, Msg).

call(Pid, Msg, Timeout) ->
  gen_server:call(Pid, Msg, Timeout).

multi_call(Name, Msg) ->
  gen_server:multi_call(Name, Msg).

multi_call(Nodes, Name, Msg) ->
  gen_server:multi_call(Nodes, Name, Msg).

multi_call(Nodes, Name, Msg, Timeout) ->
  gen_server:multi_call(Nodes, Name, Msg, Timeout).

cast(Pid, Msg) ->
  gen_server:cast(Pid, Msg).

abcast(Name, Msg) ->
  gen_server:abcast(Name, Msg).

abcast(Nodes, Name, Msg) ->
  gen_server:abcast(Nodes, Name, Msg).

reply(Pid, Msg) ->
  gen_server:reply(Pid, Msg).


%%% Initialize the server
init({listen, Callback, Args, Port, Options}) ->
  {ok, LSock}    = gen_tcp:listen(Port, [{reuseaddr, true}|Options]),
  inet:setopts(LSock, [{active, false}]),
  gen_server:cast(self(), fd_tcp_accept),
  {ok, #server{sock=LSock, callback=Callback, args=Args, port=Port, options=Options}};

init({accept, Server, LSock, Callback, Args}) ->
  gen_server:cast(self(), fd_tcp_accept),
  {ok, #accept{sock=LSock, pid=Server, callback=Callback, state=Args}};

init({connect, Callback, Args, Host, Port, Options}) ->
  {ok, Sock} = gen_tcp:connect(Host, Port, Options),
  inet:setopts(Sock, [{active, once}]),
  put(fd_tcp_sock, Sock),
  if Callback /= undefined ->
    case Callback:init(Args) of
    {ok,State} ->
      {ok, #client{sock=Sock, callback=Callback, state=State}};
    {ok,State,hibernate} ->
      {ok, #client{sock=Sock, callback=Callback, state=State}, hibernate};
    {ok,State,Timeout} ->
      {ok, #client{sock=Sock, callback=Callback, state=State}, Timeout};
    {stop,Reason} ->
      {stop, Reason};
    ignore ->
      ignore
    end;
  true ->
    {ok, #client{sock=Sock}}
  end.


%%% Handle call messages
handle_call(Msg, From, #accept{}=State) ->
  #accept{ callback=Callback, state=CallbackState } = State,
  Response = Callback:handle_call(Msg, From, CallbackState),
  do_handle_callback_response(Response, State);

handle_call(_Msg, _From, #client{callback=undefined}=State) ->
  {reply, ok, State};
  
handle_call(Msg, From, #client{}=State) ->
  #client{ callback=Callback, state=CallbackState } = State,
  Response = Callback:handle_call(Msg, From, CallbackState),
  do_handle_callback_response(Response, State);

handle_call(_Msg, _From, #server{}=State) ->
  {reply, ok, State}.


%%% Handle cast messages
handle_cast(fd_tcp_stop, State) ->
  {stop, normal, State};

handle_cast(fd_tcp_accept, #accept{}=State) ->
  #accept{sock=LSock, pid=Server, callback=Callback, state=Args} = State,
  {ok, Sock} = gen_tcp:accept(LSock),
  unlink(Server),
  gen_server:cast(Server, fd_tcp_accept),
  inet:setopts(Sock, [{active, once}]),
  put(fd_tcp_sock, Sock),
  case Callback:init(Args) of
  {ok,NewState} ->
    {noreply, #accept{sock=Sock, pid=Server, callback=Callback, state=NewState}};
  {ok,NewState,hibernate} ->
    {noreply, #accept{sock=Sock, pid=Server, callback=Callback, state=NewState}, hibernate};
  {ok,NewState,Timeout} ->
    {noreply, #accept{sock=Sock, pid=Server, callback=Callback, state=NewState}, Timeout};
  {stop,Reason} ->
    {stop, Reason, #accept{sock=Sock, pid=Server, callback=Callback, state=undefined}};
  ignore ->
    {stop, ignore, #accept{sock=Sock, pid=Server, callback=Callback, state=undefined}}
  end;

handle_cast(Msg, #accept{}=State) ->
  #accept{ callback=Callback, state=CallbackState } = State,
  Response = Callback:handle_cast(Msg, CallbackState),
  do_handle_callback_response(Response, State);

handle_cast(_Msg, #client{callback=undefined}=State) ->
  {noreply, State};

handle_cast(Msg, #client{}=State) ->
  #client{ callback=Callback, state=CallbackState } = State,
  Response = Callback:handle_cast(Msg, CallbackState),
  do_handle_callback_response(Response, State);

handle_cast(fd_tcp_accept, #server{}=State) ->
  #server{ sock=LSock, callback=Callback, args=Args } = State,
  {ok, AccPid} = gen_server:start_link(?MODULE, {accept, self(), LSock, Callback, Args}, []),
  {noreply, State#server{pid=AccPid}};

handle_cast(_Msg, #server{}=State) ->
  {noreply, State}.


%%% Handle generic messages [ACCEPT]
handle_info({tcp, Sock, Data}, #accept{sock=Sock}=State) ->
  try
    #accept{ callback=Callback, state=CallbackState } = State,
    Response = Callback:handle_data(Data, CallbackState),
    do_handle_callback_response(Response, State)
  after
    inet:setopts(Sock, [{active, once}])
  end;

handle_info({tcp_closed, Sock}, #accept{sock=Sock}=State) ->
  {stop, normal, State};

handle_info({tcp_error, Sock, Reason}, #accept{sock=Sock}=State) ->
  {stop, Reason, State};

handle_info(Info, #accept{}=State) ->
  #accept{ callback=Callback, state=CallbackState } = State,
  Response = Callback:handle_info(Info, CallbackState),
  do_handle_callback_response(Response, State);


%%% Handle generic messages [CLIENT]
handle_info({tcp, _Sock, _Data}, #client{callback=undefined}=State) ->
  {noreply, State};

handle_info({tcp, Sock, Data}, #client{sock=Sock}=State) ->
  try
    #client{ callback=Callback, state=CallbackState } = State,
    Response = Callback:handle_data(Data, CallbackState),
    do_handle_callback_response(Response, State)
  after
    inet:setopts(Sock, [{active, once}])
  end;

handle_info({tcp_closed, Sock}, #client{sock=Sock}=State) ->
  {stop, normal, State};

handle_info({tcp_error, Sock, Reason}, #client{sock=Sock}=State) ->
  {stop, Reason, State};

handle_info(_Info, #client{callback=undefined}=State) ->
  {noreply, State};

handle_info(Info, #client{}=State) ->
  #client{ callback=Callback, state=CallbackState } = State,
  Response = Callback:handle_info(Info, CallbackState),
  do_handle_callback_response(Response, State);


%%% Handle generic messages [SERVER]
handle_info({'EXIT',AccPid,_Reason}, #server{pid=AccPid}=State) ->
  gen_server:cast(self(), accept),
  {noreply, State#server{pid=undefined}};

handle_info(_Info, #server{}=State) ->
  {noreply, State}.


%%% Before stopping the server
terminate(_Reason, #server{}=State) ->
  #server{ sock = Sock, pid=AccPid } = State,
  erlang:exit(AccPid, kill),
  gen_tcp:close(Sock),
  ok;

terminate(Reason, #accept{}=State) ->
  #accept{ sock=Sock, callback=Callback, state=CallbackState } = State,
  Callback:terminate(Reason, CallbackState),
  gen_tcp:close(Sock),
  ok;

terminate(Reason, #client{}=State) ->
  #client{ sock=Sock, callback=Callback, state=CallbackState } = State,
  Callback:terminate(Reason, CallbackState),
  gen_tcp:close(Sock),
  ok.


%%% Code Changes
code_change(_OldVsn, #server{}=State, _Extra) ->
  {ok, State};

code_change(OldVsn, #accept{}=State, Extra) ->
  #accept{ callback=Callback, state=CallbackState } = State,
  {ok, NewState} = Callback:code_change(OldVsn, CallbackState, Extra),
  State#accept{ state=NewState };

code_change(OldVsn, #client{}=State, Extra) ->
  #client{ callback=Callback, state=CallbackState } = State,
  {ok, NewState} = Callback:code_change(OldVsn, CallbackState, Extra),
  State#client{ state=NewState }.


%%% Internal API
do_handle_callback_response(Response, #client{}=State) ->
  case Response of
  {reply, Reply, NewCallbackState} ->
    {reply, Reply, State#client{ state=NewCallbackState }};
  
  {reply, Reply, NewCallbackState, hibernate} ->
    {reply, Reply, State#client{ state=NewCallbackState }, hibernate};
  
  {reply, Reply, NewCallbackState, Timeout} ->
    {reply, Reply, State#client{ state=NewCallbackState }, Timeout};
  
  {noreply, NewCallbackState} ->
    {noreply, State#client{ state=NewCallbackState }};
  
  {noreply, NewCallbackState, hibernate} ->
    {noreply, State#client{ state=NewCallbackState }, hibernate};
  
  {noreply, NewCallbackState, Timeout} ->
    {noreply, State#client{ state=NewCallbackState }, Timeout};
  
  {stop, Reason, Reply, NewCallbackState} ->
    {stop, Reason, Reply, State#client{ state=NewCallbackState }};
  
  {stop, Reason, NewCallbackState} ->
    {stop, Reason, State#client{ state=NewCallbackState }}
  end;

do_handle_callback_response(Response, #accept{}=State) ->
  case Response of
  {reply, Reply, NewCallbackState} ->
    {reply, Reply, State#accept{ state=NewCallbackState }};
  
  {reply, Reply, NewCallbackState, hibernate} ->
    {reply, Reply, State#accept{ state=NewCallbackState }, hibernate};
  
  {reply, Reply, NewCallbackState, Timeout} ->
    {reply, Reply, State#accept{ state=NewCallbackState }, Timeout};
  
  {noreply, NewCallbackState} ->
    {noreply, State#accept{ state=NewCallbackState }};
  
  {noreply, NewCallbackState, hibernate} ->
    {noreply, State#accept{ state=NewCallbackState }, hibernate};
  
  {noreply, NewCallbackState, Timeout} ->
    {noreply, State#accept{ state=NewCallbackState }, Timeout};
  
  {stop, Reason, Reply, NewCallbackState} ->
    {stop, Reason, Reply, State#accept{ state=NewCallbackState }};
  
  {stop, Reason, NewCallbackState} ->
    {stop, Reason, State#accept{ state=NewCallbackState }}
  end.
