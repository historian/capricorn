-module(bertrpc_server).
-behaviour(gen_server).
-include("bertrpc.hrl").

-define(DEFAULT_PORT, 3457).

-export([start_link/2]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).
-export([accept_loop/2,loop/2]).

-record(connection,{
  socket=undefined,
  server=undefined,
  user=undefined
}).

start_link(Mod, Options) ->
  Port = proplists:get_value(port, Options, ?DEFAULT_PORT),
  ServerOptions = proplists:delete(port, Options),
  gen_server:start_link(?MODULE, {Mod, Port}, ServerOptions).

init({Mod, Port}) ->
  case gen_tcp:listen(Port,[{active,false},binary,{reuseaddr,true}]) of
  {ok, LS} ->
    spawn_link(?MODULE,accept_loop,[LS, self()]),
    {ok, {Mod, [], LS}};
  {error,Reason} -> {error,Reason}
  end.

handle_call({authenticate, Args}, _From, {Mod, Modules, LS}) ->
  User = Mod:authenticate(Args),
  {reply, User, {Mod, Modules, LS}};

handle_call({set_modules, Modules}, _From, {Mod, _, LS}) ->
  {reply, ok, {Mod, Modules, LS}};

handle_call({get_module, M}, _From, {Mod, Modules, LS}) ->
  R = 
  case proplists:get_value(M, Modules) of
  undefined -> module_not_found;
  Pid       -> {ok, Pid}
  end,
  {reply, R, {Mod, Modules, LS}}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(Info, State) ->
  io:format("E: ~p\n", [Info]),
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


accept_loop(LS, Server) ->
  case gen_tcp:accept(LS) of
  {ok,S} ->
    spawn_link(?MODULE,loop,[#connection{socket=S, server=Server}, #bertrpc_info{}]),
    accept_loop(LS, Server);
  {error, closed} -> 
    exit(normal),
    ok
  end.

loop(C, Meta) ->
  case bert_recv(C) of
  {ok, _, BERP_IN} ->
    {C1, Meta1} = handle_berp(BERP_IN, C, Meta),
    loop(C1, Meta1);
  _E ->
    exit(normal),
    ok
  end.

bert_recv(#connection{socket=S}) ->
  case gen_tcp:recv(S, 4) of
  {ok, <<0:4/big-unsigned-integer-unit:8>>} ->
    {ok, 0, <<"">>};
  {ok, <<L:4/big-unsigned-integer-unit:8>>} ->
    case gen_tcp:recv(S, L) of
    {ok, BERP_IN} -> {ok, L, BERP_IN};
    E -> E
    end;
  E -> E
  end.

bert_recv_stream(C) ->
  bert_recv_stream(C, <<"">>).

bert_recv_stream(C, Acc) ->
  case bert_recv(C) of
  {ok, 0, _} ->
    Acc;
  {ok, _, D} ->
    Acc1 = <<Acc/binary, D/binary>>,
    bert_recv_stream(C, Acc1);
  E -> E
  end.

handle_stream(_, #bertrpc_info{has_stream=false}=M) -> M;
handle_stream(C, #bertrpc_info{has_stream=true}=M) ->
  M#bertrpc_info{data=bert_recv_stream(C)}.

handle_berp(BERP_IN, C, Meta) ->
  Response = receive_berp(BERP_IN, C, Meta),
  send_bert_response(Response).

receive_berp(BERP_IN, C, Meta) ->
  try
    bert:decode(BERP_IN)
  of
    BERT_IN -> handle_bert(BERT_IN, C, Meta)
  catch
    error:_ ->
      R = {error, {protocol, 2, <<"Parse Error">>, <<"Unable to read data">>, []}},
      {send, R, C, #bertrpc_info{}}
  end.

handle_bert({call, sec, authenticate, A}, #connection{server=Server,user=undefined}=C, _Meta) ->
  try
    gen_server:call(Server, {authenticate, A}, 60000)
  of
    {ok, User} ->
      {send, {reply, {bert, true}}, C#connection{user=User}, #bertrpc_info{}};
    _ ->
      R = {error, {server, 4, <<"AuthenticationError">>, <<"Failed to authenticate">>, []}},
      {send, R, C, #bertrpc_info{}}
  catch
    error:_ ->
      R = {error, {server, 3, <<"ServerError">>, <<"ServerError">>, []}},
      {send, R, C, #bertrpc_info{}};
    throw:_ ->
      R = {error, {server, 3, <<"ServerError">>, <<"ServerError">>, []}},
      {send, R, C, #bertrpc_info{}}
  end;
handle_bert({call, sec, authenticate, _}, #connection{}=C, _Meta) ->
  {send, {reply, {bert, true}}, C, #bertrpc_info{}};
handle_bert({call, M, F, A}, C, Meta) ->
  try
    Module = get_module(C, M),
    Meta1 = handle_stream(C, Meta),
    gen_server:call(Module, {F, A, Meta1}, 60000)
  of
    Result ->
      {send, Result, C, #bertrpc_info{}}
  catch
    error:module_not_found ->
      R = {error, {protocol, 1, <<"ProtocolError">>, <<"No such module">>, []}},
      {send, R, C, #bertrpc_info{}};
    error:_ ->
      R = {error, {server, 3, <<"ServerError">>, <<"ServerError">>, []}},
      {send, R, C, #bertrpc_info{}};
    throw:_ ->
      R = {error, {server, 3, <<"ServerError">>, <<"ServerError">>, []}},
      {send, R, C, #bertrpc_info{}}
  end;
handle_bert({cast, M, F, A}, C, Meta) ->
  try
    Module = get_module(C, M),
    Meta1 = handle_stream(C, Meta),
    gen_server:cast(Module, {F, A, Meta1})
  of
    _ ->
      {send, {noreply}, C, #bertrpc_info{}}
  catch
    error:module_not_found ->
      R = {error, {protocol, 1, <<"ProtocolError">>, <<"No such module">>, []}},
      {send, R, C, #bertrpc_info{}};
    error:_ ->
      R = {error, {server, 3, <<"ServerError">>, <<"ServerError">>, []}},
      {send, R, C, #bertrpc_info{}};
    throw:_ ->
      R = {error, {server, 3, <<"ServerError">>, <<"ServerError">>, []}},
      {send, R, C, #bertrpc_info{}}
  end;
handle_bert({info, callback, Callback}, C, #bertrpc_info{callbacks=Callbacks}=Meta) ->
  {nosend, null, C, Meta#bertrpc_info{callbacks=Callbacks++[Callback]}};
handle_bert([info, stream, _], C, Meta) ->
  {nosend, null, C, Meta#bertrpc_info{has_stream=true}};
handle_bert({info, stream, _}, C, Meta) ->
  {nosend, null, C, Meta#bertrpc_info{has_stream=true}};
handle_bert({info, Command, Options}, C, #bertrpc_info{infos=Infos}=Meta) ->
  {nosend, null, C, Meta#bertrpc_info{infos=Infos++[{Command, Options}]}}.

send_bert_response({send, BERT_OUT, C, M}) ->
  send_bert(C, BERT_OUT),
  {C, M};
send_bert_response({nosend, _, C, M}) ->
  {C, M}.

send_bert(#connection{socket=S}, BERT_OUT) ->
  BERP_OUT = bert:encode(BERT_OUT),
  Size = size(BERP_OUT),
  BLOB = <<Size:4/big-unsigned-integer-unit:8>>,
  gen_tcp:send(S,[BLOB,BERP_OUT]),
  ok.


get_module(#connection{server=Server}, M) ->
  case gen_server:call(Server, {get_module, M}) of
  {ok, Pid} -> Pid;
  _ -> throw(module_not_found)
  end.