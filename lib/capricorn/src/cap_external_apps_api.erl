-module(cap_external_apps_api).
-export([handle_call/3, handle_cast/2]).



handle_call({all,
            [Node], _},
            _From, State) ->
  try
    All = cap_machine_apps:all(Node),
    {reply, {ok, All}, State}
  catch
    throw:T -> {reply, {error, T}, State}
  end;



handle_call({create,
            [Node, Name, Domains, Env], _},
            _From, State) ->
  try
    ok = cap_machine_apps:create(Node, Name, Domains, Env, []),
    {reply, true, State}
  catch
    throw:T -> {reply, {error, T}, State}
  end;



handle_call({import,
            [Node, Name, Domains, Env, Root, Gems, Uid, Gid], _},
            _From, State) ->
  try
    ok = cap_machine_apps:import(Node,
                                 Name,
                                 Domains,
                                 Env,
                                 Gems,
                                 Root,
                                 Uid,
                                 Gid),
    {reply, true, State}
  catch
    throw:T -> {reply, {error, T}, State}
  end;



handle_call({restart,
            [Node, Id], _},
            _From, State) ->
  try
    ok = cap_application:restart({Node, Id}),
    {reply, true, State}
  catch
    throw:T -> {reply, {error, T}, State}
  end;



handle_call({start,
            [Node, Id], _},
            _From, State) ->
  try
    ok = cap_application:start({Node, Id}),
    {reply, true, State}
  catch
    throw:T -> {reply, {error, T}, State}
  end;



handle_call({stop,
            [Node, Id], _},
            _From, State) ->
  try
    ok = cap_application:stop({Node, Id}),
    {reply, true, State}
  catch
    throw:T -> {reply, {error, T}, State}
  end;



handle_call({relink,
            [Node, Id], _},
            _From, State) ->
  try
    ok = cap_application:relink({Node, Id}),
    {reply, true, State}
  catch
    throw:T -> {reply, {error, T}, State}
  end;



handle_call({update,
            [Node, Id, NewDomains, NewGems], _},
            _From, State) ->
  try
    ok = cap_machine_apps:update(Node, Id, NewDomains, NewGems),
    {reply, true, State}
  catch
    throw:T -> {reply, {error, T}, State}
  end;



handle_call({fupdate,
            [Node, Id], _},
            _From, State) ->
  try
    ok = cap_machine_apps:fupdate(Node, Id),
    {reply, true, State}
  catch
    throw:T -> {reply, {error, T}, State}
  end.



handle_cast({_, _, _},
            State) ->
  {noreply, State}.


