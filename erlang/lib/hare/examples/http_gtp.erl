-module(http_gtp).
% -behaviour(hare_server).

-export([start_link/0, init/1, handle_transaction/2]).

start_link() ->
  hare_server:start_link({local, ?MODULE}, ?MODULE, [{port, 4000}]).

init(_Conn) ->
  % io:format("new connection\n", []),
  {ok, state}.

handle_transaction(T, State) ->
  io:format("~p\n", [T:get(request, headers)]),
  case T:get_request_header('connection') of
  "Upgrade" ->
    case T:get_request_header('upgrade') of
    "GTP/1.0" ->
      handle_gtp_transaction(T, State);
    _Other ->
      T:respond('426', [
        {'Upgrade',    'GTP/1.0'},
        {'Connection', 'Upgrade'}
      ]), {ok, undefined}
    end;
  _Other    ->
    T:respond('426', [
      {'Upgrade',    'GTP/1.0'},
      {'Connection', 'Upgrade'}
    ]), {ok, undefined}
  end.

handle_gtp_transaction(T, _State) ->
  "/" ++ RelPath = T:get(request, uri, path),
  Path = filename:absname(RelPath, "/Users/simon"),
  case filelib:is_dir(Path) of
  true ->
    case T:get(request, method) of
    'GET' ->
      handle_gtp_fetch_transaction(T, Path);
    'PUT' ->
      handle_gtp_update_transaction(T, Path);
    _Other ->
      T:respond('405', [
        {'Allow', 'GET, PUT'}
      ]), {ok, undefined}
    end;
  false ->
    T:respond('404', [
    ]), {ok, undefined}
  end.

handle_gtp_fetch_transaction(T, Path) ->
  io:format("~p\n", [
    [{spawn, "/usr/local/bin/git-upload-pack "++Path}, []]]),
  Port = erlang:open_port(
    {spawn, "/usr/local/bin/git-upload-pack "++Path}, []),

  io:format("port: ~p\n", [Port]),

  Stream = T:get(response, stream),
  handle_loop(Port, Stream).

handle_gtp_update_transaction(T, Path) ->
  Port = erlang:open_port(
    {spawn_executable, "/usr/local/bin/git-receive-pack"},
    [{args, [Path]}, use_stdio, exit_status]),
  Stream = T:get(response, stream),
  handle_loop(Port, Stream).

handle_loop(Port, Stream) ->
  Stream:setopts([{active, once}, {packet, raw}, {nodelay, true}]),
  receive
  {Port, {exit_status, Status}} ->
    io:format("e: ~p\n", [Status]),
    {ok, undefined};

  {Port, Else} ->
    io:format("d: ~p\n", [Else]),
    % Stream:send(Data),
    handle_loop(Port, Stream);

  {tcp, _, Data} ->
    io:format("d: ~p\n", [Data]),
    port_command(Port, Data),
    handle_loop(Port, Stream);

  {tcp_closed, _} ->
    io:format("e: socket\n", []),
    % {ok, undefined},
    handle_loop(Port, Stream);

  Else ->
    io:format("d: ~p\n", [Else])

  after 3000 ->
    io:format("timeout")
  end.

