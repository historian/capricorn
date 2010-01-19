-module(bertrpc_module).
-behaviour(gen_server).

-export([behaviour_info/1]).
-export([start/3, start/4, start_link/3, start_link/4]).

-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
  [{init,1}, {bert_call, 4}, {bert_cast, 4}, {terminate,2}, {code_change,3}];
behaviour_info(_) ->
  undefined.

start(Mod, Args, Options) ->
  gen_server:start(?MODULE, {Mod, Args}, Options).

start(Name, Mod, Args, Options) ->
  gen_server:start(Name, ?MODULE, {Mod, Args}, Options).
 
start_link(Mod, Args, Options) ->
  gen_server:start_link(?MODULE, {Mod, Args}, Options).
 
start_link(Name, Mod, Args, Options) ->
  gen_server:start_link(Name, ?MODULE, {Mod, Args}, Options).

init({Mod, Args}) ->
  case apply(Mod, init, [Args]) of
  {ok, ModState} -> {ok, {Mod, ModState}};
  Else           -> Else
  end.

handle_call({F, A, Meta}, _From, {Mod, ModState}) ->
  try
    apply(Mod, bert_call, [F, A, Meta, ModState])
  of
    {reply, Result, ModState1} -> {reply, {reply, Result}, {Mod, ModState1}};
    {error, Error,  ModState1} -> {reply, {error, Error},  {Mod, ModState1}}
  catch
    throw:Error -> {reply, {error, Error},  {Mod, ModState}}
  end.

handle_cast({F, A, Meta}, {Mod, ModState}) ->
  {noreply, ModState1} = apply(Mod, bert_cast, [F, A, Meta, ModState]),
  {noreply, {Mod, ModState1}}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(Reason, {Mod, ModState}) ->
  apply(Mod, terminate, [Reason, ModState]).

code_change(OldVsn, {Mod, ModState}, Extra) ->
  apply(Mod, code_change, [OldVsn, ModState, Extra]).
