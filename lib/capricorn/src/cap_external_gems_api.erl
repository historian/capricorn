-module(cap_external_gems_api).
-export([handle_call/3, handle_cast/2]).



handle_call({push,
            _, Info},
            _From, State) ->
  case proplists:get_value(stream, Info) of
  undefined ->
    Reason = {user, 1, <<"CapricornGemError">>,
                       <<"This is not a gem">>, []},
    {reply, {error, Reason}, State};
  
  _WeHaveAStream ->
    try
      {ok, Data} = collect_stream_data(sof, []),
      cap_cluster_gems:push(Data)
    of
    {ok,Missing} ->
      {reply, {ok,Missing}, State};
      
    {error,already_present} ->
      Reason = {user, 1, <<"CapricornGemError">>,
                         <<"This gem is already present in the cluster.">>, []},
      {reply, {error, Reason}, State};
      
    {error,{[gem_error,ErrorMessage]}} ->
      Reason = {user, 1, <<"CapricornGemError">>, ErrorMessage, []},
      {reply, {error, Reason}, State};
      
    {error,{[not_found]}} ->
      Reason = {user, 1, <<"CapricornGemError">>,
                         <<"Transfering failed!">>, []},
      {reply, {error, Reason}, State}
      
    catch
      throw:T -> {reply, {error, T}, State}
    end
  end;



handle_call({all,
            _, _},
            _From, State) ->
  try
    {ok, All} = cap_cluster_gems:all(),
    {reply, {ok, All}, State}
  catch
    throw:T -> {reply, {error, T}, State}
  end;



handle_call({missing,
            _, _},
            _From, State) ->
  try
    {ok, Missing} = cap_cluster_gems:missing(),
    {reply, {ok, Missing}, State}
  catch
    throw:T -> {reply, {error, T}, State}
  end.



handle_cast(_, State) ->
  {noreply, State}.



collect_stream_data(sof, Acc) ->
  collect_stream_data(bertrpc:stream(), Acc);

collect_stream_data({ok, eof}, Acc) ->
  {ok, list_to_binary(lists:reverse(Acc))};

collect_stream_data({error, Reason}, _) ->
  {error, Reason};

collect_stream_data({ok, Data}, Acc) ->
  collect_stream_data(bertrpc:stream(), [Data|Acc]).


