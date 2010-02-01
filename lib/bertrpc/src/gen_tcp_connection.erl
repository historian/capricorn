-module(gen_tcp_connection).
-behaviour(gen_server).


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


%%% External API
behaviour_info(callbacks) ->
  [{init,1}, {handle_data, 2}, {handle_info, 2}, {terminate,2}, {code_change,3}];
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


%%% Initialize the server
init({listen, Callback, Args, Port, Options}) ->
  {ok, LSock}    = gen_tcp:listen(Port, [], Options),
  inet:setopts(LSock, [{active, false}]),
  gen_server:cast(self(), accept),
  {ok, #server{sock=LSock, callback=Callback, args=Args, port=Port, options=Options}};

init({accept, Server, LSock, Callback, Args}) ->
  {ok, Sock} = gen_tcp:accept(LSock),
  unlink(Server),
  gen_server:cast(Server, accept),
  inet:setopts(Sock, [{active, true}]),
  {ok, State} = Callback:init(Args),
  {ok, #accept{sock=Sock, pid=Server, callback=Callback, state=State}};


%%% Handle call messages
handle_call(_Msg, _From, State) ->
  {reply, ok, State}.


%%% Handle cast messages
handle_cast(accept, #server{}=State) ->
  #server{ sock=LSock, callback=Callback, args=Args } = State,
  {ok, AccPid} = gen_server:start_link(?MODULE, {accept, self(), LSock, Callback, Args}, []),
  {noreply, State#server{pid=AccPid}};

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(_Msg, State) ->
  {noreply, State}.


%%% Handle generic messages
handle_info({tcp, Sock, Data}, #accept{sock=Sock}=State) ->
  #accept{ callback=Callback, state=CallbackState } = State,
  case Callback:handle_data(Data, CallbackState) of
  {ok, NewCallbackState} ->
    {noreply, State#accept{ state=NewCallbackState }};
  {stop, Reason, NewCallbackState} ->
    {stop, Reason, State#accept{ state=NewCallbackState }}
  end;

handle_info({tcp_closed, Sock}, #accept{sock=Sock}=State) ->
  {stop, normal, State};

handle_info({tcp_error, Sock, Reason}, #accept{sock=Sock}=State) ->
  {stop, Reason, State};
  
handle_info(Info, #accept{}=State) ->
  #accept{ callback=Callback, state=CallbackState } = State,
  Callback:handle_info(Info, CallbackState);

handle_info({'EXIT',AccPid,Reason}, #server{pid=AccPid}=State) ->
  gen_server:cast(self(), accept),
  {noreply, State#server{pid=undefined}};
  
handle_info(_Info, State) ->
  {noreply, State}.


%%% Before stopping the server
terminate(_Reason, #server{}=State) ->
  #server{ sock = Sock } = State,
  gen_tcp:close(Sock),
  ok.

terminate(Reason, #accept{}=State) ->
  #accept{ sock=Sock, callback=Callback, state=CallbackState } = State,
  Callback:terminate(Reason, CallbackState),
  gen_tcp:close(Sock),
  ok.


%%% Code Changes
code_change(_OldVsn, #server{}=State, _Extra) ->
  {ok, State};

code_change(OldVsn, #accept{}=State, Extra) ->
  #accept{ callback=Callback, state=CallbackState } = State,
  Callback:code_change(OldVsn, CallbackState, Extra).


%%% Internal API
