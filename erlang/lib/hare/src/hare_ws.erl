-module(hare_ws).

-export([send/2]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).



send(Type, Message) when Type > 127 ->
  Socket = erlang:get(hare_ws_socket),
  B_Message = hare_utils:to_b(Message),
  gen_tcp:send(<<
    Type:8/unsigned-integer,
    encode_length(erlang:size(B_Message), <<"">>)/binary,
    B_Message/binary>>);

send(Type, Message) ->
  Socket = erlang:get(hare_ws_socket),
  gen_tcp:send(<<
    Type:8/unsigned-integer,
    hare_utils:to_b(Message)/binary,
    255:8/unsigned-integer>>).



init({Callback, Request}) ->
  case Callback:init(Request) of
  {ok, State} ->
    {ok, {Callback, State, <<"">>}};

  {ok, State, hibernate} ->
    {ok, {Callback, State, <<"">>}, hibernate};

  {ok, State, Timeout} ->
    {ok, {Callback, State, <<"">>}, Timeout};

  {stop, Reason} ->
    {stop, Reason};

  ignore ->
    ignore
  end.



handle_call(Msg, From, {Callback, State, Buffer}) ->
  case Callback:handle_call(Msg, From, State) of
  {reply, Reply, NewState} ->
    {reply, Reply, {Callback, NewState, Buffer}};

  {reply, Reply, NewState, hibernate} ->
    {reply, Reply, {Callback, NewState, Buffer}, hibernate};

  {reply, Reply, NewState, Timeout} ->
    {reply, Reply, {Callback, NewState, Buffer}, Timeout};

  {noreply, NewState} ->
    {noreply, {Callback, NewState, Buffer}};

  {noreply, NewState, hibernate} ->
    {noreply, {Callback, NewState, Buffer}, hibernate};

  {noreply, NewState, Timeout} ->
    {noreply, {Callback, NewState, Buffer}, Timeout};

  {stop, Reason, Reply, NewState} ->
    {stop, Reason, Reply, {Callback, NewState, Buffer}};

  {stop, Reason, NewState} ->
    {stop, Reason, {Callback, NewState, Buffer}};
  end.



handle_cast(Msg, {Callback, State, Buffer}) ->
  case Callback:handle_cast(Msg, State) of
  {noreply, NewState} ->
    {noreply, {Callback, NewState, Buffer}};

  {noreply, NewState, hibernate} ->
    {noreply, {Callback, NewState, Buffer}, hibernate};

  {noreply, NewState, Timeout} ->
    {noreply, {Callback, NewState, Buffer}, Timeout};

  {stop, Reason, NewState} ->
    {stop, Reason, {Callback, NewState, Buffer}};
  end.



handle_info({tcp, _, Data}, {Callback, State, Buffer}) ->
  case buffer_data(Buffer, Data) of
  {ok, Message, NewBuffer} ->
    case Callback:handle_message(Message, State) of
    {noreply, NewState} ->
      {noreply, {Callback, NewState, NewBuffer}};

    {noreply, NewState, hibernate} ->
      {noreply, {Callback, NewState, NewBuffer}, hibernate};

    {noreply, NewState, Timeout} ->
      {noreply, {Callback, NewState, NewBuffer}, Timeout};

    {stop, Reason, NewState} ->
      {stop, Reason, {Callback, NewState, NewBuffer}}
    end;

  {ok, NewBuffer} ->
    {noreply, {Callback, State, NewBuffer}}

  end;


handle_info(Msg, {Callback, State, Buffer}) ->
  case Callback:handle_info(Msg, State) of
  {noreply, NewState} ->
    {noreply, {Callback, NewState, Buffer}};

  {noreply, NewState, hibernate} ->
    {noreply, {Callback, NewState, Buffer}, hibernate};

  {noreply, NewState, Timeout} ->
    {noreply, {Callback, NewState, Buffer}, Timeout};

  {stop, Reason, NewState} ->
    {stop, Reason, {Callback, NewState, Buffer}}
  end.



terminate(Reason, {Callback, State, _Buffer}) ->
  Callback:terminate(Reason, State).



code_change(OldVsn, {Callback, State, Buffer}, Extra) ->
  {ok, NewState} = Callback:code_change(OldVsn, State, Extra),
  {ok, {Callback, NewState, Buffer}}.



buffer_data(Buffer, Chunk) when is_list(Chunk) ->
  buffer_data(Buffer, list_to_binary(Chunk));

buffer_data(Buffer, Chunk) ->
  NewBuffer1 = <<Buffer/binary, Chunk/binary>>,
  case NewBuffer1 of
  <<1:1/integer, Type:7/unsigned-integer, Rest1/binary>> ->
    case read_binary(Rest1, 0) of
    {ok, Message, NewBuffer2} ->
      {ok, {Type, Message}, NewBuffer2};
    read_more ->
      {ok, NewBuffer1}
    end;

  <<0:1/integer, Type:7/unsigned-integer, Rest1/binary>> ->
    case read_utf8(Rest1, <<"">>) of
    {ok, Message, NewBuffer2} ->
      {ok, {Type, Message}, NewBuffer2};
    read_more ->
      {ok, NewBuffer1}
    end

  end.



read_binary(<<"">>, _T) ->
  read_more;

read_binary(<<0:1/integer, L:7/unsigned-integer, Rest/binary>>, T) ->
  Length = (T * 128) + L;
  case Rest of
  <<Message:Lenght/binary, NewBuffer/binary>> ->
    {ok, Message, NewBuffer};

  _Other ->
    read_more
  end;

read_binary(<<1:1/integer, L:7/unsigned-integer, Rest/binary>>, T) ->
  read_binary(Rest, (T * 128) + L).



read_utf8(<<"">>, _Acc) ->
  read_more;

read_utf8(<<255:8/unsigned-integer, Rest/binary>>, Acc) ->
  {ok, Acc, Rest};

read_utf8(<<Byte:1/binary, Rest/binary>>, Acc) ->
  read_utf8(Rest, <<Acc/binary, Byte/binary>>).



encode_length(0, Acc) ->
  Acc;

encode_length(Length, Acc) ->
  C = Length rem 128,
  case erlang:size(Acc) of
  Size when Size > 0 ->
    encode_length(erlang:trunc((Length - C) / 128), <<1:1/unsigned-integer, C:7/unsigned-integer, Acc/binary>>);
  Size when Size = 0 ->
    encode_length(erlang:trunc((Length - C) / 128), <<0:1/unsigned-integer, C:7/unsigned-integer, Acc/binary>>)
  end.