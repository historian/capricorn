-module(cap_service_users).
-behaviour(bertrpc_module).
-include("bertrpc.hrl").
-include("capricorn.hrl").

-export([start_link/0]).
-export([init/1, bert_call/4, bert_cast/4, terminate/2, code_change/3]).

start_link() ->
  bertrpc_module:start_link(?MODULE, [], []).


init([]) ->
  {ok, state}.


bert_call(auth, [Addr, Login, Token], _, State) ->
  try
    cap_application:service(Addr, [users, auth, [Login, Token]])
  of
    {error, Error} ->
      ?LOG_DEBUG("~s:~s(...) => ~p", [?MODULE, auth, Error]),
      {error, Error, State};
    ok             -> {reply, ok, State}
  catch
    throw:T -> {error, T, State}
  end;

bert_call(push, [Addr, Attributes], _, State) ->
  try
    cap_application:service(Addr, [users, push, [Attributes]])
  of
    {error, Error} ->
      ?LOG_DEBUG("~s:~s(...) => ~p", [?MODULE, push, Error]),
      {error, Error, State};
    ok             -> {reply, ok, State}
  catch
    throw:T -> {error, T, State}
  end;

bert_call(remove, [Addr, Login], _, State) ->
  try
    cap_application:service(Addr, [users, remove, [Login]])
  of
    {error, Error} ->
      ?LOG_DEBUG("~s:~s(...) => ~p", [?MODULE, remove, Error]),
      {error, Error, State};
    ok             -> {reply, ok, State}
  catch
    throw:T -> {error, T, State}
  end.


bert_cast(_, _, _Extra, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
