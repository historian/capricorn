-module(bertrpc_connection).
-behaviour(fd_tcp).


-export([call/4, call/5, cast/4, info/3]).

-export([listen_link/4, listen_link/3, listen/4, listen/3]).
-export([connect_link/3, connect_link/2, connect/3, connect/2]).

-export([behaviour_info/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_data/2,
         handle_info/2, terminate/2, code_change/3]).


-type client_ref()   :: {pid(), ref()} .
-type request_info() :: {atom(), [term()]} .
-type action()       :: {'call' | 'cast', atom(), atom(), [term()]}
-type response()     :: {'error', term()} | {'reply', term()} | {'noreply'} .

-record(request, {
  action    :: action(),
  infos=[]  :: [request_info()],
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
  {noreply, try_send_request({Request, From}, State)}.


%%% Handle cast messages
handle_cast({info, stream, [Data]}, State) when is_binary(Data) ->
  
  fd_tcp:send(self(), bert:encode({info, stream, []})),
  
  {noreply, State#state { data = Data }};

handle_cast({info, stream, [Path]}, State) when is_list(Path) ->
  
  {ok, Data} = file:read_file(Path),
  fd_tcp:send(self(), bert:encode({info, stream, []})),
  
  {noreply, State#state { data = Data }};

handle_cast({info, Command, Options}, State) ->
  
  fd_tcp:send(self(), bert:encode({info, Command, Options})),
  
  {noreply, State};

handle_cast({send_stream}, State) ->
  #state{ data=Data } = State,
  
  if is_binary(Data) ->
    fd_tcp:send(self(), Data);
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
do_handle_info(Command, Options, #state{}=State) ->
  #state{ req_in = Request } = State,
  #request{ infos = Infos } = Request,
  
  State#state{
    req_in = Request#request{ infos = [{Command, Options}|Infos] }
  }.


do_handle_request(State) ->
  #state{ req_in = Request } = State,
  #request{ action = Action, infos = Infos } = Request,
  {Type, Module, Function, Arguments} = Action
  
  #state{ callback = Callback, state = CallbackState } = State,
  
  % put socket in pasive mode
  % <-
  
  case Type of
  call ->
    case Callback:handle_call({Module, Function, Arguments, Infos}, wire, CallbackState) of
    {reply, {error, Reason}, NewState} ->
      fd_tcp:send(self(), bert:encode({error, Reason})),
      State2 = State#state{ req_in = undefined, state = NewState },
      {noreply, State2};
    
    {reply, Response, NewState} ->
      fd_tcp:send(self(), bert:encode({reply, Response})),
      State2 = State#state{ req_in = undefined, state = NewState },
      {noreply, State2};
    
    {reply, {error, Reason}, NewState, hibernate} ->
      fd_tcp:send(self(), bert:encode({error, Reason})),
      State2 = State#state{ req_in = undefined, state = NewState },
      {noreply, State2, hibernate};
    
    {reply, Response, NewState, hibernate} ->
      fd_tcp:send(self(), bert:encode({reply, Response})),
      State2 = State#state{ req_in = undefined, state = NewState },
      {noreply, State2, hibernate};
    
    {reply, {error, Reason}, NewState, Timeout} ->
      fd_tcp:send(self(), bert:encode({error, Reason})),
      State2 = State#state{ req_in = undefined, state = NewState },
      {noreply, State2, Timeout};
    
    {reply, Response, NewState, Timeout} ->
      fd_tcp:send(self(), bert:encode({reply, Response})),
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
      fd_tcp:send(self(), bert:encode({error, E})),
      State2 = State#state{ req_in = undefined, state = NewState },
      {stop, Reason, State2};
      
    {stop, Reason, Reply, NewState} ->
      fd_tcp:send(self(), bert:encode({reply, Reply})),
      State2 = State#state{ req_in = undefined, state = NewState },
      {stop, Reason, State2};
    
    {stop, Reason, NewState} ->
      fd_tcp:send(self(), bert:encode({error, Reason})),
      State2 = State#state{ req_in = undefined, state = NewState },
      {stop, Reason, State2}
    end
  cast ->
    fd_tcp:send(self(), bert:encode({noreply})),
    
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
  fd_tcp:reply(Client, {ok, Result}),
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


try_send_request(queued, #state{req_out=undefined}=State) ->
  #state{ queue = Queue1 } = State,
  case queue:out(Queue1) of
  {{value, Request}, Queue2} ->
    #request { action = Action, infos = Infos } = Request,
    
    do_send_info(Infos),
    do_send_action(Action),
    do_send_stream(Infos),
    
    State#state{ queue = Queue2, req_out = Request };
  {empty, _} ->
    
    State
  end;

try_send_request({Request, From}, #state{req_out=undefined}=State) ->
  #request { action = Action, infos = Infos } = Request,
  
  do_send_info(Infos),
  do_send_action(Action),
  do_send_stream(Infos),
  
  State#state{
    req_out = Request#request{ client = From }
  };

try_send_request({Request, From}, #state{}=State) ->
  #state{ queue = Queue1 } = State,
  
  Request2 = Request#request{ client = From },
  Queue2   = queue:in(Request2, Queue1),
  
  State#state{ queue = Queue2 }.


do_send_info([]) ->
  ok;

do_send_info([{stream, [Data]}|Rest]) ->
  fd_tcp:send(bert:encode({info, stream, []})),
  do_send_info(Rest);

do_send_info([{Command, Options}|Rest]) ->
  fd_tcp:send(bert:encode({info, Command, Options})),
  do_send_info(Rest).


do_send_action(Action) ->
  fd_tcp:send(bert:encode(Action)).


do_send_stream([]) ->
  ok;

do_send_stream([{stream, [Data]}|Rest]) when is_binary(Data) ->
  fd_tcp:send(Data),
  fd_tcp:send(<<"">>),
  do_send_stream(Rest);

do_send_stream([{stream, [Data]}|Rest]) when is_list(Data) ->
  [fd_tcp:send(Chunk) || Chunk <- Data],
  fd_tcp:send(<<"">>),
  do_send_stream(Rest);

do_send_stream([_|Rest]) ->
  do_send_stream(Rest);

