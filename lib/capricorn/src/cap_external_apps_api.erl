-module(cap_external_apps_api).
-behaviour(bertrpc_module).
-include("bertrpc/include/bertrpc.hrl").

-export([start_link/0]).
-export([init/1, bert_call/4, bert_cast/4, terminate/2, code_change/3]).

start_link() ->
  bertrpc_module:start_link(?MODULE, [], []).

init([]) ->
  {ok, state}.

bert_call(all, [Node], _, State) ->
  try
    All = cap_machine_apps:all(Node),
    {reply, {ok,All}, State}
  catch
    throw:T -> {error, T, State}
  end;

bert_call(create, [Node, Name, Domains, Environment], _, State) ->
  try
    ok = cap_machine_apps:create(Node, Name, Domains, Environment, []),
    {reply, true, State}
  catch
    throw:T -> {error, T, State}
  end;

bert_call(import, [Node, Name, Domains, Environment, Root, Gems, Uid, Gid], _, State) ->
  try
    ok = cap_machine_apps:import(Node, Name, Domains, Environment, Gems, Root, Uid, Gid),
    {reply, true, State}
  catch
    throw:T -> {error, T, State}
  end;

bert_call(restart, [Node, Id], _, State) ->
  try
    ok = cap_application:restart({Node, Id}),
    {reply, true, State}
  catch
    throw:T -> {error, T, State}
  end;

bert_call(start, [Node, Id], _, State) ->
  try
    ok = cap_application:start({Node, Id}),
    {reply, true, State}
  catch
    throw:T -> {error, T, State}
  end;

bert_call(stop, [Node, Id], _, State) ->
  try
    ok = cap_application:stop({Node, Id}),
    {reply, true, State}
  catch
    throw:T -> {error, T, State}
  end;

bert_call(relink, [Node, Id], _, State) ->
  try
    ok = cap_application:relink({Node, Id}),
    {reply, true, State}
  catch
    throw:T -> {error, T, State}
  end;

bert_call(update, [Node, Id, NewDomains, NewGems], _, State) ->
  try
    ok = cap_machine_apps:update(Node, Id, NewDomains, NewGems),
    {reply, true, State}
  catch
    throw:T -> {error, T, State}
  end;

bert_call(fupdate, [Node, Id], _, State) ->
  try
    ok = cap_machine_apps:fupdate(Node, Id),
    {reply, true, State}
  catch
    throw:T -> {error, T, State}
  end.
  

bert_cast(_, _, _Extra, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.