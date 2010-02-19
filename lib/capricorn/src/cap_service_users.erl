-module(cap_service_users).
-include("capricorn.hrl").
-export([handle_call/3, handle_cast/2]).



handle_call({auth,
            [Addr, Login, Token], _},
            _From, State) ->
  try
    cap_application:service(Addr, [users, auth, [Login, Token]])
  of
    {error, Error} ->
      ?LOG_DEBUG("~s:~s(...) => ~p", [?MODULE, auth, Error]),
      {reply, {error, Error}, State};
    ok ->
      {reply, ok, State}
  catch
    throw:T -> {reply, {error, T}, State}
  end;



handle_call({push,
            [Addr, Attributes], _},
            _From, State) ->
  try
    cap_application:service(Addr, [users, push, [Attributes]])
  of
    {error, Error} ->
      ?LOG_DEBUG("~s:~s(...) => ~p", [?MODULE, push, Error]),
      {reply, {error, Error}, State};
    ok ->
      {reply, ok, State}
  catch
    throw:T -> {reply, {error, T}, State}
  end;



handle_call({remove,
            [Addr, Login], _},
            _From, State) ->
  try
    cap_application:service(Addr, [users, remove, [Login]])
  of
    {error, Error} ->
      ?LOG_DEBUG("~s:~s(...) => ~p", [?MODULE, remove, Error]),
      {reply, {error, Error}, State};
    ok ->
      {reply, ok, State}
  catch
    throw:T -> {reply, {error, T}, State}
  end.



handle_cast(_, State) ->
  {noreply, State}.


