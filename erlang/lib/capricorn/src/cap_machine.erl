%%
%% Supervised worker process module
%%
%% File   : cap_machine.erl
%% Created: 2010-01-04
%%
%% @author simonmenke <simon.menke@gmail.com>
%% @copyright 2010 simonmenke
%%
%% @doc TODO make nice description
%%

-module(cap_machine).
-author('simonmenke <simon.menke@gmail.com>').
-include("capricorn.hrl").
-behaviour(gen_server).

%% external api
-export([config/1]).

%% operation & maintenance api
-export([start_link/0, stop/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).
-export([wait_for_cluster/2, monitor_cluster/2]).
-export([ensure_gems_are_present_for_app/1]).

-record(ctx, {
  cluster,
  knows_cluster=false,
  gems_path,
  installed_gems
}).

config(Node) ->
  lists:sort(gen_server:call({cap_config, Node}, all)).

%%
%% Operation & Maintenance API
%%

%% @spec start_link() -> {ok, Pid}
%% @doc Start the cap_machine
start_link() ->
  gen_server:start_link({local, cap_machine}, ?MODULE, [], []).

stop() ->
  gen_server:cast(cap_machine, stop).

ensure_gems_are_present_for_app(App) ->
  gen_server:call(cap_machine, {ensure_gems_are_present_for_app, App}, 900000).

%%
%% Genserver callback functions
%%

%% @spec init(State) -> {ok, State}
%% @doc Callback for initialize the cap_machine
init([]) ->
  Node = cap_config:get({node, node()}, "cluster.address", 'cluster@cluster'),
  ep2p:join(Node, [{{127,0,0,1}, 4567}], erlang:get_cookie()),
  KnowsCluster =
  case net_adm:ping(Node) of
  pong ->
    monitor_cluster(Node),
    ?LOG_INFO("Connected to ~s", [Node]),
    true;
  pang ->
    ?LOG_INFO("Cannot connect to ~s", [Node]),
    wait_for_cluster(Node),
    false
  end,

  GemsPath = cap_config:get({node, node()}, "gems.path", "/opt/local/lib/ruby/gems/1.8"),

  InstalledGems = ets:new(gems, [set,private,{keypos,2}]),

  emq:new(machine_queue, [{size, 1}]),

  {ok, #ctx{
    knows_cluster  = KnowsCluster,
    cluster        = Node,
    gems_path      = GemsPath,
    installed_gems = InstalledGems
  }}.

%% @spec handle_call(_Request, _From, State) -> {reply, Reply, State}
%% @doc Callback for synchronous requests
handle_call({ensure_gems_are_present_for_app, App}, _From, State) ->
  R = do_ensure_gem_for_app(App, State),
  {reply, R, State};
handle_call(_Request, _From, State) ->
  Reply = ok,
  {reply, Reply, State}.

%% @spec handle_cast(stop, State) -> {stop, normal, State}
%% @doc Callback for assynchronous messages
handle_cast(stop, State) ->
  {stop, normal, State};
handle_cast({cluster_up, Node}, State) ->
  ?LOG_INFO("Cluster came up: ~s", [Node]),
  monitor_cluster(Node),
  {noreply, State#ctx{knows_cluster=true}};
handle_cast({cluster_down, Node}, State) ->
  ?LOG_INFO("Cluster went down: ~s", [Node]),
  wait_for_cluster(Node),
  {noreply, State#ctx{knows_cluster=false}};
handle_cast(_Msg, State) ->
  {noreply, State}.

%% @spec handle_info(_Info, State) -> {noreply, State}
%% @doc Callback for timeout or other unreconized messages
handle_info(_Info, State) ->
  {noreply, State}.

%% @spec terminate(_Reason, _State) -> ok
%% @doc Callback for free resources used by the server
terminate(_Reason, _State) ->
  ok.

%% @spec code_change(_OldVsn, State, _Extra) -> {ok, State}
%% @doc Callback for upgrade source code
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

wait_for_cluster(Node) ->
  spawn_link(?MODULE, wait_for_cluster, [Node, self()]).
wait_for_cluster(Node, Owner) ->
  case net_adm:ping(Node) of
  pong ->
    gen_server:cast(Owner, {cluster_up, Node});
  pang ->
    receive after 5000 -> wait_for_cluster(Node, Owner) end
  end.

monitor_cluster(Node) ->
  spawn_link(?MODULE, monitor_cluster, [Node, self()]).
monitor_cluster(Node, Owner) ->
  erlang:monitor_node(Node, true),
  receive
  {nodedown, Node} ->
    gen_server:cast(Owner, {cluster_down, Node})
  end.



do_ensure_gem_for_app(App, Ctx) ->
  RequiredGems = App#application.required_gems,
  case do_install_gems(RequiredGems, Ctx) of
  {ok, Installed} ->
    {ok, App#application{
      installed_gems = uniq_list(Installed)
    }};
  Error -> Error
  end.



do_install_gems(Gems, Ctx) ->
  lists:foldl(fun
  (Gem, {ok, Acc}) ->
    case do_install_gem(Gem, Ctx) of
    {ok, Installed} ->
      {ok, Acc ++ Installed};
    Error -> Error
    end;
  (_, Error) ->
    Error
  end, {ok, []}, Gems).

do_install_gem(#gem{}=Spec, Ctx) ->
  do_install_gem(deps, Spec, Ctx);

do_install_gem(NameOrDep, Ctx) ->
  do_install_gem(lookup, NameOrDep, Ctx).

do_install_gem(lookup, {DepName,_}=Dep, #ctx{cluster=Cluster}=Ctx) ->
  case cap_cluster_gems:lookup(Cluster, Dep) of
  {ok, Spec} ->
    ?LOG_DEBUG("found gem ~s", [DepName]),
    do_install_gem(deps, Spec, Ctx);

  {error, not_found} ->
    ?LOG_DEBUG("error ~p", [{missing_gem, DepName}]),
    {error, {missing_gem, Dep}}
  end;

do_install_gem(lookup, GemName, #ctx{cluster=Cluster}=Ctx) ->
  case cap_cluster_gems:lookup(Cluster, GemName) of
  {ok, Spec} ->
    ?LOG_DEBUG("found gem ~s", [GemName]),
    do_install_gem(deps, Spec, Ctx);

  {error, not_found} ->
    ?LOG_DEBUG("error ~p", [{missing_gem, GemName}]),
    {error, {missing_gem, GemName}}
  end;

do_install_gem(deps, #gem{deps=Deps}=Spec, Ctx) ->
  Installed = lists:foldl(fun
  (Dep, Acc) when is_list(Acc) ->
    case do_install_gem(lookup, Dep, Ctx) of
    {ok, CurrentInstalled} ->
      Acc ++ CurrentInstalled;
    Error -> Error
    end;
  (_, Error) ->
    Error
  end, [], Deps),

  case Installed of
  Installed when is_list(Installed) ->
    case do_install_gem(check, Spec, Ctx) of
    {ok, Installed2} ->
      {ok, Installed ++ Installed2};
    Error -> Error
    end;

  Error ->
    ?LOG_DEBUG("error ~p", [Error]),
    Error
  end;

do_install_gem(check, #gem{}=Spec, Ctx) ->
  GemName = (Spec#gem.id)#gem_id.name,
  case do_is_gem_installed(Spec, Ctx) of
  true  ->
    ?LOG_DEBUG("allready installed gem ~s", [GemName]),
    {ok, [Spec#gem.id]};
  false ->
    do_install_gem(pull, Spec, Ctx);
  {error, E} ->
    ?LOG_DEBUG("error ~p", [E]),
    {error, E}
  end;

do_install_gem(pull, #gem{}=Spec, #ctx{cluster=Cluster}=Ctx) ->
  GemName = (Spec#gem.id)#gem_id.name,
  ?LOG_DEBUG("pull gem ~s", [GemName]),
  case cap_cluster_gems:pull(Cluster, Spec) of
  {ok, Data} ->

    ?LOG_DEBUG("staging gem ~s", [GemName]),
    case file:write_file("/tmp/capricorn-gem.gem", Data) of
    ok ->

      do_install_gem(install, Spec, Ctx);

    Error ->
      ?LOG_DEBUG("error ~p", [Error]),
      Error
    end;

  Error ->
    ?LOG_DEBUG("error ~p", [Error]),
    Error
  end;

do_install_gem(install, #gem{}=Spec, _Ctx) ->
  GemName = (Spec#gem.id)#gem_id.name,
  Cmd = "install --no-rdoc --no-ri --local --no-update-sources "++
        "/tmp/capricorn-gem.gem",

  ?LOG_INFO("installing gem ~s", [GemName]),

  case gem_exec(Cmd) of
  "Successfully"++_ ->
    {ok, [Spec#gem.id]};

  Error ->
    ?LOG_DEBUG("error ~p", [Error]),
    {error, {install_failed, GemName}}
  end.



-spec do_is_gem_installed(gem_spec(), #ctx{}) -> true | false | {error, badarg} .
do_is_gem_installed(#gem{}=Gem, #ctx{installed_gems=T, gems_path=GemsPath}) ->
  case ets:lookup(T, Gem#gem.id) of
  [] ->
    Name1    = (Gem#gem.id)#gem_id.name,
    Name2    = binary_to_list(Name1),

    Version1 = (Gem#gem.id)#gem_id.version,
    case Version1 of
    undefined ->
      Version2 = "*",
      Fullname = lists:flatten([Name2, "-", Version2]),
      Path     = filename:join([GemsPath, "gems", Fullname]),

      case filelib:wildcard(Path) of
      []    -> false;
      _Else ->
        ets:insert(T, Gem),
        true
      end;
    _Else ->
      Version2 = cap_cluster_gems:version_to_string(Version1),
      Fullname = lists:flatten([Name2, "-", Version2]),
      Path     = filename:join([GemsPath, "gems", Fullname]),

      case filelib:is_file(Path) of
      false -> false;
      true  ->
        ets:insert(T, Gem),
        true
      end
    end;
  _Else -> true
  end.

gem_exec(Args) ->
  os:cmd(lists:flatten([gem_exe(), " " |Args])).

gem_exe() ->
  case get(gem_exe) of
  Cmd when is_list(Cmd) -> Cmd;
  _ ->
    case os:find_executable("gem") of
    false   -> throw({error, missing_cmd, "gem"});
    GemPath ->
      put(gem_exe, GemPath),
      GemPath
    end
  end.



uniq_list(List) ->
  lists:foldl(fun(Elem, Acc) ->
    case lists:member(Elem, Acc) of
    true  -> Acc;
    false -> Acc ++ [Elem]
    end
  end, [], List).


