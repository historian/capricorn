-module(scratch).

-export([start_link/0, handle_transaction/1]).

start_link() ->
  hare:listen({local, ?MODULE}, ?MODULE, [{port, 9000}]).



handle_transaction(Request) ->
  {200, [{'Content-Type', 'text/plain'}, {'Content-Length', 11}], [
    "Hello World"
  ]}.



% handle_upgrade(Request, Socket) ->
%   {close}.
%
%
%
% accept_websocket(Request) ->
%   {accept, State}.
%
% handle_websocket(Socket, State) ->
%   Socket:activate(),
%   receive
%   {ws, Data} ->
%     [To, Msg] = json:parse(Data),
%     list_to_atom("ws_client."++To) ! Msg,
%     {ok, Socket};
%
%   {ws_close} ->
%     {close};
%
%   stop ->
%     {close};
%
%   Msg ->
%     Data = json:dump(Msg),
%     Socket:send(Data),
%     {ok, State}
%
%   after 1000 * 60 * 20 ->
%     {close}
%   end.