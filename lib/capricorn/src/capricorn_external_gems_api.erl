-module(capricorn_external_gems_api).
-behaviour(bertrpc_module).
-include("bertrpc.hrl").

-export([start_link/0]).
-export([init/1, bert_call/4, bert_cast/4, terminate/2, code_change/3]).

start_link() ->
  bertrpc_module:start_link(?MODULE, [], []).

init([]) ->
  {ok, state}.

bert_call(push, _, #bertrpc_info{data=undefined}, State) ->
  {error, {user, 1, <<"CapricornGemError">>,
                    <<"This is not a gem">>, []}, State};
bert_call(push, _, #bertrpc_info{data=Data}, State) ->
  try
    capricorn_cluster_gems:push(Data)
  of
  {ok,Missing} ->
    {reply, {ok,Missing}, State};
  {error,already_present} ->
    {error, {user, 1, <<"CapricornGemError">>, <<"This gem is already present in the cluster.">>, []}, State}
  catch
    throw:T -> {error, T, State}
  end;
bert_call(all, _, _, State) ->
  try
    {ok,All} = capricorn_cluster_gems:all(),
    {reply, {ok,All}, State}
  catch
    throw:T -> {error, T, State}
  end;
bert_call(missing, _, _, State) ->
  try
    {ok,Missing} = capricorn_cluster_gems:missing(),
    {reply, {ok,Missing}, State}
  catch
    throw:T -> {error, T, State}
  end.

bert_cast(_, _, _Extra, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.