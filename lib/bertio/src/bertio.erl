-module(bertio).

-export([open_port/2, port_close/1]).
-export([send/2, recv/1, recv/2]).

open_port(PortName, PortOptions) ->
  erlang:open_port(PortName, [{packet, 4}, nouse_stdio, binary|PortOptions]).

send(Port, BERT) ->
  BERP = bert:encode(BERT),
  erlang:port_command(Port, BERP).

recv(Port) ->
  recv(Port, 1000).

recv(Port, Timeout) ->
  receive
  {Port, {data, BERP}} ->
    {bert, bert:decode(BERP)}
  after Timeout ->
    erlang:error(timeout)
  end.

port_close(Port) ->
  send(Port, stop),
  erlang:port_close(Port).
