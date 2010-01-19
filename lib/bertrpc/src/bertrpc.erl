-module(bertrpc).
-behaviour(supervisor).

-export([behaviour_info/1]).

-export([start_link/3, start_link/4]).
-export([start_modules/1, init/1]).

-spec behaviour_info(atom()) -> 'undefined' | [{atom(), arity()}].
behaviour_info(callbacks) ->
  [{init,1}, {authenticate, 1}, {authorize, 2}];
behaviour_info(_) ->
  undefined.

start_link(Mod, Args, Options) ->
  case supervisor:start_link(?MODULE, {Mod, Args, Options}) of
  {ok, Pid} ->
    send_module_pids_to_server(Pid),
    {ok, Pid};
  Else -> Else
  end.

start_link(ServerName, Mod, Args, Options) ->
  case supervisor:start_link(ServerName, ?MODULE, {Mod, Args, Options}) of
  {ok, Pid} ->
    send_module_pids_to_server(Pid),
    {ok, Pid};
  Else -> Else
  end.

send_module_pids_to_server(Sup) ->
  Children = supervisor:which_children(Sup),
  ServerPid = child_with_id(Children, bertrpc_server),
  ModSupPid = child_with_id(Children, bertrpc_modules),
  ModChildren = supervisor:which_children(ModSupPid),
  Modules = children_with_pids(ModChildren, []),
  gen_server:call(ServerPid, {set_modules, Modules}).

child_with_id([{Id,Child,_,_}|_], Id) ->
  Child;
child_with_id([], _) ->
  undefined;
child_with_id([_|Rest], Id) ->
  child_with_id(Rest, Id).

children_with_pids([{Id,Child,_,_}|Rest], Acc) ->
  children_with_pids(Rest, Acc++[{Id, Child}]);
children_with_pids([], Acc) ->
  Acc.

child_specs(Mod, ModuleSpecs, Options) ->
  {{one_for_all, 10, 3600},[
    {bertrpc_server,
      {bertrpc_server, start_link, [Mod, Options]},
      permanent,
      brutal_kill,
      worker,
      [bertrpc_server]},
    {bertrpc_modules,
      {bertrpc, start_modules, [ModuleSpecs]},
      permanent,
      infinity,
      supervisor,
      [bertrpc]}
  ]}.

start_modules(ModuleSpecs) ->
  ChildSpecs = {{one_for_one, 10, 3600},build_module_specs(ModuleSpecs, [])},
  supervisor:start_link(?MODULE, ChildSpecs).
  
build_module_specs([ModuleSpec|Rest], Acc) ->
  {Id,{M,F,A},Restart,Shutdown} = ModuleSpec,
  ChildSpec = {Id,{M,F,A},Restart,Shutdown,worker,[M]},
  build_module_specs(Rest, Acc++[ChildSpec]);
build_module_specs([], Acc) -> Acc.

init({Mod, Args, Options}) ->
  {ok, ModuleSpecs} = Mod:init(Args),
  ChildSpecs = child_specs(Mod, ModuleSpecs, Options),
  {ok, ChildSpecs};
init(ChildSpecs) ->
  {ok, ChildSpecs}.