-module(hare_utils).

-export([to_s/1, to_b/1, to_a/1, to_h/1, normalize_h/1, to_uri/1, response_reason/1, receive_request/1, send_response/2]).



to_s(Atom) when is_atom(Atom) ->
  atom_to_list(Atom);
to_s(List) when is_list(List) ->
  lists:flatten([to_s(I) || I <- List]);
to_s(Binary) when is_binary(Binary) ->
  binary_to_list(Binary);
to_s(Integer) when is_integer(Integer) ->
  Integer.



to_b(Any) ->
  list_to_binary(to_s(Any)).



to_a(Any) ->
  list_to_atom(to_s(Any)).



to_h(Header) ->
  list_to_atom(string:to_lower(to_s(Header))).



normalize_h(Header) when is_binary(Header) ->
  normalize_h_char(binary_to_list(Header), []);
normalize_h(Header) when is_atom(Header) ->
  normalize_h_char(atom_to_list(Header), []);
normalize_h(Header) ->
  normalize_h_char(Header, []).

normalize_h_char([], Acc) ->
  lists:reverse(Acc);
normalize_h_char([Char |Rest], []=Acc) ->
  normalize_h_char(Rest, [string:to_upper(Char) |Acc]);
normalize_h_char([$-, Char |Rest], Acc) ->
  normalize_h_char(Rest, [string:to_upper(Char), $- |Acc]);
normalize_h_char([$_, Char |Rest], Acc) ->
  normalize_h_char(Rest, [string:to_upper(Char), $_ |Acc]);
normalize_h_char([$., Char |Rest], Acc) ->
  normalize_h_char(Rest, [string:to_upper(Char), $. |Acc]);
normalize_h_char([Char |Rest], Acc) ->
  normalize_h_char(Rest, [string:to_lower(Char) |Acc]).



to_uri(Any) ->
  parse_path(to_s(Any), [], {undefined, undefined, undefined}).

parse_path([], Acc, {_,B,C}) ->
  {lists:reverse(Acc), B,C};
parse_path([$? | Rest], Acc, {_,B,C}) ->
  parse_query(Rest, [], {lists:reverse(Acc), B,C});
parse_path([$# | Rest], Acc, {_,B,C}) ->
  parse_fragment(Rest, [], {lists:reverse(Acc), B,C});
parse_path([Char | Rest], Acc, URI) ->
  parse_path(Rest, [Char | Acc], URI).

parse_query([], Acc, {A,_,C}) ->
  {A,lists:reverse(Acc),C};
parse_query([$# | Rest], Acc, {A,_,C}) ->
  parse_fragment(Rest, [], {A,lists:reverse(Acc),C});
parse_query([Char | Rest], Acc, URI) ->
  parse_query(Rest, [Char | Acc], URI).

parse_fragment(Fragment, _Acc, {A,B,_}) ->
  {A,B,Fragment}.



response_reason(100) -> "Continue" ;
response_reason(101) -> "Switching Protocols" ;
response_reason(102) -> "Processing" ;
response_reason(200) -> "OK" ;
response_reason(201) -> "Created" ;
response_reason(202) -> "Accepted" ;
response_reason(203) -> "Non-Authoritative Information" ;
response_reason(204) -> "No Content" ;
response_reason(205) -> "Reset Content" ;
response_reason(206) -> "Partial Content" ;
response_reason(207) -> "Multi-Status" ;
response_reason(300) -> "Multiple Choices" ;
response_reason(301) -> "Moved Permanently" ;
response_reason(302) -> "Found" ;
response_reason(303) -> "See Other" ;
response_reason(304) -> "Not Modified" ;
response_reason(305) -> "Use Proxy" ;
response_reason(306) -> "Switch Proxy" ;
response_reason(307) -> "Temporary Redirect" ;
response_reason(400) -> "Bad Request" ;
response_reason(401) -> "Unauthorized" ;
response_reason(402) -> "Payment Required" ;
response_reason(403) -> "Forbidden" ;
response_reason(404) -> "Not Found" ;
response_reason(405) -> "Method Not Allowed" ;
response_reason(406) -> "Not Acceptable" ;
response_reason(407) -> "Proxy Authentication Required" ;
response_reason(408) -> "Request Timeout" ;
response_reason(409) -> "Conflict" ;
response_reason(410) -> "Gone" ;
response_reason(411) -> "Length Required" ;
response_reason(412) -> "Precondition Failed" ;
response_reason(413) -> "Request Entity Too Large" ;
response_reason(414) -> "Request-URI Too Long" ;
response_reason(415) -> "Unsupported Media Type" ;
response_reason(416) -> "Requested Range Not Satisfiable" ;
response_reason(417) -> "Expectation Failed" ;
response_reason(421) -> "There are too many connections from your internet address" ;
response_reason(422) -> "Unprocessable Entity" ;
response_reason(423) -> "Locked" ;
response_reason(424) -> "Failed Dependency" ;
response_reason(425) -> "Unordered Collection" ;
response_reason(426) -> "Upgrade Required" ;
response_reason(449) -> "Retry With" ;
response_reason(450) -> "Blocked by Windows Parental Controls" ;
response_reason(500) -> "Internal Server Error" ;
response_reason(501) -> "Not Implemented" ;
response_reason(502) -> "Bad Gateway" ;
response_reason(503) -> "Service Unavailable" ;
response_reason(504) -> "Gateway Timeout" ;
response_reason(505) -> "HTTP Version Not Supported" ;
response_reason(506) -> "Variant Also Negotiates" ;
response_reason(507) -> "Insufficient Storage" ;
response_reason(509) -> "Bandwidth Limit Exceeded" ;
response_reason(510) -> "Not Extended" ;
response_reason(530) -> "User access denied" .



normalize_h_for_env(Header) when is_atom(Header) ->
  normalize_h_for_env(atom_to_list(Header));
normalize_h_for_env(Header) ->
  normalize_h_for_env("HTTP_" ++ Header, []).

normalize_h_for_env([], Acc) ->
  list_to_atom(lists:reverse(Acc));
normalize_h_for_env([$-|Rest], Acc) ->
  normalize_h_for_env(Rest, [$_|Acc]);
normalize_h_for_env([Char|Rest], Acc) ->
  normalize_h_for_env(Rest, [string:to_upper(Char)|Acc]).



receive_request(Socket) ->
  inet:setopts(Socket, [{packet, http}, {active, once}]),
  receive_request(Socket, []).

receive_request(Socket, Acc) ->
  receive

  {_, _, {http_request, Method, {abs_path, Uri}, _Version}} ->

    {Path, Query, Fragment} = hare_utils:to_uri(Uri),

    inet:setopts(Socket, [{packet, httph}, {active, once}]),

    receive_request(Socket, [
      {'REQUEST_METHOD', to_a(Method)},
      {'PATH_INFO',      Path},
      {'QUERY',          Query},
      {'Fragment',       Fragment}
    | Acc]);

  {_, _, {http_header, _, Field, _, Value}} ->
    inet:setopts(Socket, [{packet, httph}, {active, once}]),

    receive_request(Socket, [
      {normalize_h_for_env(Field), Value}
    | Acc]);

  {_, _, http_eoh} ->
    inet:setopts(Socket, [{packet, raw}]),
    dict:from_list(Acc);

  {_, _, {http_error, String}} ->
    gen_tcp:close(Socket),
    io:format("http error: ~p\n", [String]),
    erlang:error(http_error);

  {tcp_closed, _} ->
    case Acc of
    [] ->
      gen_tcp:close(Socket),
      erlang:exit(normal);

    Acc ->
      gen_tcp:close(Socket),
      io:format("http error: ~p\n", [unexpected_end_of_stream]),
      erlang:error(http_error)
    end;

  Other ->
    gen_tcp:close(Socket),
    io:format("http error: ~p\n", [Other]),
    erlang:error(http_error)

  after 3000 ->
    io:format("closing connection\n", []),
    gen_tcp:close(Socket),
    erlang:exit(normal)

  end.



send_response(Socket, {Status, Headers}) when is_list(Headers) ->
  gen_tcp:send(Socket, [
    "HTTP/1.1 ", integer_to_list(Status), " ", response_reason(Status), "\r\n",
    [case Header of
    {Field, Value} when is_integer(Value) ->
       [normalize_h(Field), ": ", integer_to_list(Value), "\r\n"];
    {Field, Value} when is_atom(Value) ->
       [normalize_h(Field), ": ", atom_to_list(Value), "\r\n"];
    {Field, Value} ->
       [normalize_h(Field), ": ", Value, "\r\n"]
    end || Header <- Headers],
    "\r\n"
  ]),

  ok;

send_response(Socket, {Status, Headers}) ->
  send_response(Socket, {Status, dict:to_list(Headers)});

send_response(Socket, {Status, Headers, Body}) ->
  send_response(Socket, {Status, Headers}),
  gen_tcp:send(Socket, to_s(Body)),
  ok.


