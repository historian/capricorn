-module(cap_internal_apps_api).
-behaviour(bertrpc_module).
-include("bertrpc/include/bertrpc.hrl").

-export([start_link/0]).
-export([init/1, bert_call/4, bert_cast/4, terminate/2, code_change/3]).

start_link() ->
  bertrpc_module:start_link(?MODULE, [], []).

init([]) ->
  {ok, state}.


bert_call(call, [Node, Id, Module, Function, Args], _, State) ->
  try
    cap_machine_apps:call(Node, Id, Module, Function, Args)
  of
    {ok, Result}   -> {reply, Result, State};
    {error, Error} -> {error, Error, State}
  catch
    throw:T -> {error, T, State}
  end;
bert_call(cast, [Node, Id, Module, Function, Args], _, State) ->
  try
    cap_machine_apps:cast(Node, Id, Module, Function, Args)
  of
    {ok, Result}   -> {reply, Result, State};
    {error, Error} -> {error, Error, State}
  catch
    throw:T -> {error, T, State}
  end.

bert_cast(_, _, _Extra, State) ->
  {noreply, State}.


terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.
