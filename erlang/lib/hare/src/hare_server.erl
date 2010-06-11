-module(hare_server).

-export([start_link/3]).
-export([init/4, init_conn/3]).



start_link(Name, Callback, Options) ->
  proc_lib:start_link(?MODULE, init, [
    self(),
    Name,
    Callback,
    Options]).



init(Parent, Name, Callback, Options) ->
  case Name of
  {local,  Atom} -> erlang:register(Atom, self());
  {global, Atom} -> global:register_name(Atom, self())
  end,

  Port = proplists:get_value(port, Options, 80),
  IP   = proplists:get_value(ip,   Options, {0,0,0,0}),

  {ok, Listener} = gen_tcp:listen(Port, [
    {ip,        IP},
    {active,    false},
    {backlog,   150},
    {reuseaddr, true}]),

  proc_lib:init_ack(Parent, {ok, self()}),

  accept_loop(Listener, Callback).



accept_loop(Listener, Callback) ->
  proc_lib:start(?MODULE, init_conn, [
    self(),
    Callback,
    Listener]),

  accept_loop(Listener, Callback).



init_conn(Server, Callback, Listener) ->
  {ok, Socket}  = gen_tcp:accept(Listener),
  proc_lib:init_ack(Server, {ok, self()}),
  erlang:unlink(Server),

  try connection_loop(Server, Callback, Socket)
  catch
  exit:normal ->
    erlang:exit(normal);
  Kind:Reason ->
    io:format("~p\nat ~p\n", [{Kind, Reason}, erlang:get_stacktrace()]),
    erlang:exit(error)
  end.




connection_loop(Server, Callback, Socket) ->
  Request = hare_utils:receive_request(Socket),

  case dict:find('HTTP_UPGRADE', Request) of
  {ok, "WebSocket"} ->
    handle_websocket_connection(Server, Callback, Socket, Request);

  {ok, _} ->
    handle_upgrade(Server, Callback, Socket, Request);

  _Other ->
    handle_request(Server, Callback, Socket, Request),
    connection_loop(Server, Callback, Socket)
  end.



handle_websocket_connection(_Server, Callback, Socket, Request) ->
  case Callback:accept_websocket(Request) of

  {ok, Module, State} ->
    gen_server:enter_loop(Module, [], State);

  {ok, Module, State, Timeout} when is_integer(Timeout) ->
    gen_server:enter_loop(Module, [], State, Timeout);

  {ok, Module, State, Name} ->
    case Name of
    {local,  N} -> erlang:register(N, self());
    {global, N} -> global:register_name(N, self())
    end,
    gen_server:enter_loop(Module, [], State, Name);

  {ok, Module, State, Name, Timeout} ->
    case Name of
    {local,  N} -> erlang:register(N, self());
    {global, N} -> global:register_name(N, self())
    end,
    gen_server:enter_loop(Module, [], State, Name, Timeout);

  Else ->
    gen_tcp:close(Socket),
    erlang:error(Else)

  end.



handle_upgrade(_Server, _Callback, _Socket, _Request) ->
  ok.



handle_connect(_Server, _Callback, _Socket, _Request) ->
  ok.



handle_request(_Server, Callback, Socket, Request) ->
  {Status, Headers, Body} = Callback:handle_transaction(Request),
  hare_utils:send_response(Socket, {Status, Headers, Body}).


