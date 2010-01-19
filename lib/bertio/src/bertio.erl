-module(bertio).

-export([open_port/2, port_close/1]).
-export([send/2, recv/1, recv/2]).

open_port(PortName, PortOptions) ->
  erlang:open_port(PortName, [{packet, 4}, use_stdio, binary|PortOptions]).

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

% -spec it_loop(pid(),port()) -> no_return().
% it_loop(Owner, Port) ->
%   receive
%   {bert_cmd, BERT} ->
%     BERP = bert:encode(BERT),
%     port_command(Port, BERP),
%     it_loop(Owner, Port);
%   {Port, {data, Data}} ->
%     BERT = bert:decode(Data),
%     Owner ! {self(), {bert, BERT}},
%     it_loop(Owner, Port);
%   {Port, {exit_status,Status}} ->
%     Owner ! {self(),{exit_status,Status}},
%     io:format("exit port: ~p\n", [Port]),
%     exit(port_terminated);
%   {'EXIT', Port, _Reason} ->
%     io:format("exit port: ~p\n", [Port]),
%     exit(port_terminated);
%   {'EXIT', Owner, _Reason} ->
%     io:format("exit owner: ~p\n", [Owner]),
%     send(Port, stop),
%     port_close(Port),
%     exit(normal)
%   end.
