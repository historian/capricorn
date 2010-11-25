-module(cap_gems).

-export ([
  put/3,
  get/2, get/3,
  delete/2
]).

put(Name, Version, {path, Path}) ->
  case file:open(Path, [read, raw, binary]) of
  {ok, IO} ->
    put(Name, Version, fun() ->
      case file:read(IO, 8 * 1024) of
      {ok, Data} ->
        {send, Data};
      eof ->
        file:close(IO),
        eof;
      {error, Reason} ->
        {error, Reason}
      end
    end);
  Else ->
    Else
  end;

put(Name, Version, StreamFun) ->
  {ok, C}    = riak:local_client(),
  Fullname   = list_to_binary(
    io_lib:format("_gems/~s-~s.gem", [Name, Version])),
  case luwak_file:exists(C, Fullname) of
  {ok, false} ->
    Attributes = dict:from_list([
      {name, list_to_binary(Name)},
      {version, list_to_binary(Version)}
    ]),
    {ok, File} = luwak_file:create(C, Fullname, Attributes),
    PutStream  = luwak_put_stream:start(C, File, 0, 1000),
    put_loop(C, Fullname, File, PutStream, StreamFun);
  
  {ok, true} ->
    {error, already_exists};
  
  Else ->
    Else
    
  end.

put_loop(C, Fullname, File, Stream, StreamFun) ->
  case catch StreamFun() of
  {send, Data} when is_list(Data) ->
    luwak_put_stream:send(Stream, list_to_binary(Data)),
    put_loop(C, Fullname, File, Stream, StreamFun);
  {send, Data} when is_binary(Data) ->
    luwak_put_stream:send(Stream, Data),
    put_loop(C, Fullname, File, Stream, StreamFun);
  {eof, Data} when is_list(Data) ->
    luwak_put_stream:send(Stream, list_to_binary(Data)),
    luwak_put_stream:close(Stream),
    luwak_put_stream:status(Stream, 1000);
  {eof, Data} when is_binary(Data) ->
    luwak_put_stream:send(Stream, Data),
    luwak_put_stream:close(Stream),
    luwak_put_stream:status(Stream, 1000);
  {eof} ->
    luwak_put_stream:close(Stream),
    luwak_put_stream:status(Stream, 1000);
  eof ->
    luwak_put_stream:close(Stream),
    luwak_put_stream:status(Stream, 1000);
  Else ->
    luwak_file:delete(C, Fullname),
    {error, {stream_error, Else}}
  end.

get(Name, Version) ->
  {ok, C}    = riak:local_client(),
  Fullname   = list_to_binary(
    io_lib:format("_gems/~s-~s.gem", [Name, Version])),
  case luwak_file:exists(C, Fullname) of
  {ok, true} ->
    {ok, File} = luwak_file:get(C, Fullname),
    {ok, luwak_file:get_attributes(File)};
  
  {ok, false} ->
    {error, not_found};
  
  Else ->
    Else
    
  end.

get(Name, Version, StreamFun) ->
  {ok, C}    = riak:local_client(),
  Fullname   = list_to_binary(
    io_lib:format("_gems/~s-~s.gem", [Name, Version])),
  case luwak_file:exists(C, Fullname) of
  {ok, true} ->
    {ok, File} = luwak_file:get(C, Fullname),
    GetStream  = luwak_get_stream:start(C, File, 0, 
      luwak_file:length(C, File)),
    get_loop(C, Fullname, File, GetStream, StreamFun);
  
  {ok, false} ->
    {error, not_found};
  
  Else ->
    Else
    
  end.

get_loop(C, Fullname, File, Stream, StreamFun) ->
  Result =
  case luwak_get_stream:recv(Stream, 1000) of
  {Data, Length} when is_binary(Data) ->
    try
      StreamFun(data, Data, Length),
      {ok, continue}
    catch
      T:E -> {T,E}
    end;
  eos ->
    try
      StreamFun(eof),
      {ok, stop}
    catch
      T:E -> {T,E}
    end;
  closed           -> {ok, stop};
  {error, timeout} -> {error, timeout}
  end,
  
  case Result of
  {ok, continue} ->
    get_loop(C, Fullname, File, Stream, StreamFun);
  {ok, stop} ->
    luwak_get_stream:close(Stream),
    ok;
  Else ->
    luwak_get_stream:close(Stream),
    Else
  end.

delete(Name, Version) ->
  {ok, C}    = riak:local_client(),
  Fullname   = list_to_binary(
    io_lib:format("_gems/~s-~s.gem", [Name, Version])),
  case luwak_file:exists(C, Fullname) of
  {ok, true} ->
    luwak_file:delete(C, Fullname),
    ok;
  
  {ok, false} ->
    ok;
  
  Else ->
    Else
    
  end.
