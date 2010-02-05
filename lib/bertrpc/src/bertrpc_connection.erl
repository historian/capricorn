-module(bertrpc_connection).
-behaviour(fd_tcp).


-export([call/3, call/4, call/5, call/6, cast/4, info/2, reply/2]).

-export([listen_link/4, listen_link/3, listen/4, listen/3]).
-export([connect_link/3, connect_link/2, connect/3, connect/2]).

-export([behaviour_info/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_data/2,
         handle_info/2, terminate/2, code_change/3]).


-type client_ref()   :: {pid(), reference()} .
-type request_info() :: {atom(), [term()]} .
-type action()       :: {'call' | 'cast', atom(), atom(), [term()]} .
% -type response()     :: {'error', term()} | {'reply', term()} | {'noreply'} .

-record(request, {
  action    :: action(),
  infos=[]  :: [request_info()],
  stream    :: 'undefined' | [binary()] | binary(),
  client    :: client_ref()
}).
-type request() :: #request{} .

-record(state, {
  callback :: 'undefined' | atom(),
  state    :: term(),
  
  req_in   :: 'undefined' | request(),
  req_out  :: 'undefined' | request(),
  queue    :: queue()
}).
-type state() :: #state{} .


-define(SOCK_OPTS, [binary, {packet, 4}]).


%%% Behaviour API
behaviour_info(callbacks) ->
  [{init,1}, {handle_call, 3}, {handle_cast, 2}, {handle_info, 2}, {terminate,2}, {code_change,3}];
behaviour_info(_) ->
  undefined.


%%% Start the server
listen_link(Name, Callback, Args, Port) ->
  fd_tcp:listen_link(Name, ?MODULE, {Callback, Args}, Port, ?SOCK_OPTS).

listen_link(Callback, Args, Port) ->
  fd_tcp:listen_link(?MODULE, {Callback, Args}, Port, ?SOCK_OPTS).


listen(Name, Callback, Args, Port) ->
  fd_tcp:listen(Name, ?MODULE, {Callback, Args}, Port, ?SOCK_OPTS).

listen(Callback, Args, Port) ->
  fd_tcp:listen(?MODULE, {Callback, Args}, Port, ?SOCK_OPTS).


%%% Start the client
connect_link(Name, Host, Port) ->
  fd_tcp:connect_link(Name, ?MODULE, {}, Host, Port, ?SOCK_OPTS).

connect_link(Host, Port) ->
  fd_tcp:connect_link(?MODULE, {}, Host, Port, ?SOCK_OPTS).


connect(Name, Host, Port) ->
  fd_tcp:connect(Name, ?MODULE, {}, Host, Port, ?SOCK_OPTS).

connect(Host, Port) ->
  fd_tcp:connect(?MODULE, {}, Host, Port, ?SOCK_OPTS).


%%% External Process API
call(Name, Module, Function, Arguments) ->
  call(Name, Module, Function, Arguments, []).

call(Name, Module, Function, Arguments, Infos) ->
  call(Name, Module, Function, Arguments, Infos, undefined).

call(Name, Module, Function, Arguments, Infos, Stream) ->
  fd_tcp:call(Name, #request{
    action = {call, Module, Function, Arguments},
    infos  = Infos,
    stream = Stream
  }).


cast(Name, Module, Function, Arguments) ->
  cast(Name, Module, Function, Arguments, []).

cast(Name, Module, Function, Arguments, Infos) ->
  cast(Name, Module, Function, Arguments, Infos, undefined).

cast(Name, Module, Function, Arguments, Infos, Stream) ->
  fd_tcp:call(Name, #request{
    action = {cast, Module, Function, Arguments},
    infos  = Infos,
    stream = Stream
  }).


reply(wire, {error, Reason}) ->
  send_chunks(bert:encode({error, Reason}));

reply(wire, Response) ->
  send_chunks(bert:encode({reply, Response}));

reply(Client, Response) ->
  fd_tcp:reply(Client, Response).


%%% Internal Process API
-spec info(atom(), [term()]) -> ok .
info(Command, Options) ->
  send_chunks(bert:encode({info, Command, Options})).


-spec call(atom(), atom(), [term()]) -> term() | {error, term()} .
call(Module, Function, Arguments) ->
  send_chunks(bert:encode({call, Module, Function, Arguments})).


-spec cast(atom(), atom(), [term()]) -> ok .
cast(Module, Function, Arguments) ->
  send_chunks(bert:encode({cast, Module, Function, Arguments})).


-spec stream([binary()] | binary()) -> ok .
stream(Chunks) ->
  send_chunks(Chunks),
  send_chunks(eof).


-spec send_chunks([binary()] | binary() | eof | undefined) -> ok .
send_chunks(Chunk) when is_binary(Chunk) ->
  fd_tcp:send(Chunk);

send_chunks(eof) ->
  send_chunks(<<"">>);

send_chunks(undefined) ->
  ok;

send_chunks(Chunks) when is_list(Chunks) ->
  [send_chunks(Chunk) || Chunk <- Chunks].


%%% Initialize the server
init({Callback, Args}) ->
  case Callback:init(Args) of
  {ok, SubState} ->
    {ok, #state{callback=Callback, state=SubState, queue=queue:new()}};
  
  {ok, SubState, hibernate} ->
    {ok, #state{callback=Callback, state=SubState, queue=queue:new()}, hibernate};
  
  {ok, SubState, Timeout} ->
    {ok, #state{callback=Callback, state=SubState, queue=queue:new()}, Timeout};
  
  {stop, Reason} ->
    {stop, Reason};
  
  ignore ->
    ignore
  end;
  
init({}) ->
  {ok, #state{queue=queue:new()}}.


%%% Handle call messages
-spec handle_call(request(), client_ref(), state()) -> {noreply, state()}.
handle_call(#request{}=Request, From, #state{}=State) ->
  {noreply, try_send_request({Request, From}, State)};

handle_call(Request, From, State) ->
  #state{ callback=Callback, state=CallbackState } = State,
  Response = Callback:handle_call(Request, From, CallbackState),
  do_handle_callback_response(Response, State).


%%% Handle cast messages
handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(Msg, State) ->
  #state{ callback=Callback, state=CallbackState } = State,
  Response = Callback:handle_cast(Msg, CallbackState),
  do_handle_callback_response(Response, State).


%%% Handle generic messages
handle_info(Info, State) ->
  #state{ callback=Callback, state=CallbackState } = State,
  Response = Callback:handle_info(Info, CallbackState),
  do_handle_callback_response(Response, State).


%%% Handle data messages
handle_data(Data, #state{ req_in=undefined }=State) ->
  handle_data(Data, State#state{req_in = #request{}});

handle_data(<<"">>, #state{}=State) ->
  {noreply, State};

handle_data(Data, #state{}=State) ->
  case bert:decode(Data) of
  {call, _, _, _} = Action -> 
    #state { req_in = Request1 } = State,
    State2 = State#state { req_in = Request1#request { action = Action } },
    do_handle_request(State2);
  {cast, _, _, _} = Action -> 
    #state { req_in = Request1 } = State,
    State2 = State#state { req_in = Request1#request { action = Action } },
    do_handle_request(State2);
  {info, Command, Options} -> 
    {noreply, do_handle_info(Command, Options, State)};
  {reply, Result} -> 
    {noreply, do_handle_reply(Result, State)};
  {error, Reason} -> 
    {noreply, do_handle_error(Reason, State)};
  {noreply} -> 
    {noreply, do_handle_noreply(State)}
  end.


%%% Before stopping the server
terminate(_Reason, _State) ->
  ok.


%%% Code Changes
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


%%% Internal API
do_handle_info(Command, Options, #state{}=State) ->
  #state{ req_in = Request } = State,
  #request{ infos = Infos } = Request,
  
  State#state{
    req_in = Request#request{ infos = [{Command, Options}|Infos] }
  }.


do_handle_request(State) ->
  #state{ req_in = Request } = State,
  #request{ action = Action, infos = Infos } = Request,
  {Type, Module, Function, Arguments} = Action,
  
  #state{ callback = Callback, state = CallbackState } = State,
  
  case Type of
  call ->
    case Callback:handle_call({Module, Function, Arguments, Infos}, wire, CallbackState) of
    {reply, {error, Reason}, NewState} ->
      fd_tcp:send(bert:encode({error, Reason})),
      State2 = State#state{ req_in = undefined, state = NewState },
      {noreply, State2};
    
    {reply, Response, NewState} ->
      fd_tcp:send(bert:encode({reply, Response})),
      State2 = State#state{ req_in = undefined, state = NewState },
      {noreply, State2};
    
    {reply, {error, Reason}, NewState, hibernate} ->
      fd_tcp:send(bert:encode({error, Reason})),
      State2 = State#state{ req_in = undefined, state = NewState },
      {noreply, State2, hibernate};
    
    {reply, Response, NewState, hibernate} ->
      fd_tcp:send(bert:encode({reply, Response})),
      State2 = State#state{ req_in = undefined, state = NewState },
      {noreply, State2, hibernate};
    
    {reply, {error, Reason}, NewState, Timeout} ->
      fd_tcp:send(bert:encode({error, Reason})),
      State2 = State#state{ req_in = undefined, state = NewState },
      {noreply, State2, Timeout};
    
    {reply, Response, NewState, Timeout} ->
      fd_tcp:send(bert:encode({reply, Response})),
      State2 = State#state{ req_in = undefined, state = NewState },
      {noreply, State2, Timeout};
    
    {noreply, NewState} ->
      State2 = State#state{ state = NewState },
      {noreply, State2};
    
    {noreply, NewState, hibernate} ->
      State2 = State#state{ state = NewState },
      {noreply, State2, hibernate};
    
    {noreply, NewState, Timeout} ->
      State2 = State#state{ state = NewState },
      {noreply, State2, Timeout};
    
    {stop, Reason, {error, E}, NewState} ->
      fd_tcp:send(bert:encode({error, E})),
      State2 = State#state{ req_in = undefined, state = NewState },
      {stop, Reason, State2};
      
    {stop, Reason, Reply, NewState} ->
      fd_tcp:send(bert:encode({reply, Reply})),
      State2 = State#state{ req_in = undefined, state = NewState },
      {stop, Reason, State2};
    
    {stop, Reason, NewState} ->
      fd_tcp:send(bert:encode({error, Reason})),
      State2 = State#state{ req_in = undefined, state = NewState },
      {stop, Reason, State2}
    end;
  cast ->
    fd_tcp:send(bert:encode({noreply})),
    
    case Callback:handle_cast({Module, Function, Arguments, Infos}, CallbackState) of
    {noreply, NewState} ->
      State2 = State#state{ state = NewState },
      {noreply, State2};
    
    {noreply, NewState, hibernate} ->
      State2 = State#state{ state = NewState },
      {noreply, State2, hibernate};
    
    {noreply, NewState, Timeout} ->
      State2 = State#state{ state = NewState },
      {noreply, State2, Timeout};
    
    {stop, Reason, NewState} ->
      State2 = State#state{ req_in = undefined, state = NewState },
      {stop, Reason, State2}
    end
  end.


do_reset_out_state(State) ->
  State#state{req_out=undefined}.


do_handle_reply(Result, State1) ->
  #state{ req_out = Request } = State1,
  #request{ client = Client } = Request,
  fd_tcp:reply(Client, Result),
  State2 = do_reset_out_state(State1),
  try_send_request(queued, State2).


do_handle_error(Reason, State1) ->
  #state{ req_out = Request } = State1,
  #request{ client = Client } = Request,
  fd_tcp:reply(Client, {error, Reason}),
  State2 = do_reset_out_state(State1),
  try_send_request(queued, State2).


do_handle_noreply(State1) ->
  #state{ req_out = Request } = State1,
  #request{ client = Client } = Request,
  fd_tcp:reply(Client, ok),
  State2 = do_reset_out_state(State1),
  try_send_request(queued, State2).


% send Request from the queue (if there is one)
try_send_request(queued, #state{req_out=undefined}=State) ->
  #state{ queue = Queue1 } = State,
  case queue:out(Queue1) of
  {{value, Request}, Queue2} ->
    #request { action = Action, infos = Infos, stream = Stream } = Request,
    
    [begin {Command, Options} = Info, info(Command, Options) end || Info <- Infos],
    case Action of
    {call, Module, Function, Arguments} ->
      call(Module, Function, Arguments);
    {cast, Module, Function, Arguments} ->
      cast(Module, Function, Arguments)
    end,
    stream(Stream),
    
    State#state{ queue = Queue2, req_out = Request };
  {empty, _} ->
    
    State
  end;

% send a request directly
try_send_request({Request, From}, #state{req_out=undefined}=State) ->
  #request { action = Action, infos = Infos, stream = Stream } = Request,
  
  [begin {Command, Options} = Info, info(Command, Options) end || Info <- Infos],
  case Action of
  {call, Module, Function, Arguments} ->
    call(Module, Function, Arguments);
  {cast, Module, Function, Arguments} ->
    cast(Module, Function, Arguments)
  end,
  stream(Stream),
  
  State#state{
    req_out = Request#request{ client = From }
  };

% put a request on the queue
try_send_request({Request, From}, #state{}=State) ->
  #state{ queue = Queue1 } = State,
  
  Request2 = Request#request{ client = From },
  Queue2   = queue:in(Request2, Queue1),
  
  State#state{ queue = Queue2 }.


do_handle_callback_response(Response, #state{}=State) ->
  case Response of
  {reply, Reply, NewCallbackState} ->
    {reply, Reply, State#state{ state=NewCallbackState }};
  
  {reply, Reply, NewCallbackState, hibernate} ->
    {reply, Reply, State#state{ state=NewCallbackState }, hibernate};
  
  {reply, Reply, NewCallbackState, Timeout} ->
    {reply, Reply, State#state{ state=NewCallbackState }, Timeout};
  
  {noreply, NewCallbackState} ->
    {noreply, State#state{ state=NewCallbackState }};
  
  {noreply, NewCallbackState, hibernate} ->
    {noreply, State#state{ state=NewCallbackState }, hibernate};
  
  {noreply, NewCallbackState, Timeout} ->
    {noreply, State#state{ state=NewCallbackState }, Timeout};
  
  {stop, Reason, Reply, NewCallbackState} ->
    {stop, Reason, Reply, State#state{ state=NewCallbackState }};
  
  {stop, Reason, NewCallbackState} ->
    {stop, Reason, State#state{ state=NewCallbackState }}
  end.
