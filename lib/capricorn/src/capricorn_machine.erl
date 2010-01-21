%%
%% Supervised worker process module
%%
%% File   : capricorn_machine.erl
%% Created: 2010-01-04
%%
%% @author simonmenke <simon.menke@gmail.com>
%% @copyright 2010 simonmenke
%%
%% @doc TODO make nice description
%%

-module(capricorn_machine).
-author('simonmenke <simon.menke@gmail.com>').
-include("capricorn.hrl").
-behaviour(gen_server).

%% external api
-export([config/1]).

%% operation & maintenance api
-export([start_link/0]).
 
%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).
-export([wait_for_cluster/2, monitor_cluster/2]).
-export([ensure_gems_are_present_for_app/1]).

-record(ctx, {
  cluster,
  knows_cluster=false,
  installed_gems
}).

config(Node) ->
  lists:sort(gen_server:call({capricorn_config, Node}, all)).

%%
%% Operation & Maintenance API
%%

%% @spec start_link() -> {ok, Pid}
%% @doc Start the capricorn_machine
start_link() ->
  gen_server:start_link({local, capricorn_machine}, ?MODULE, [], []).

ensure_gems_are_present_for_app(App) ->
  gen_server:call(capricorn_machine, {ensure_gems_are_present_for_app, App}, 180000).

%%
%% Genserver callback functions
%%

%% @spec init(State) -> {ok, State}
%% @doc Callback for initialize the capricorn_machine
init([]) ->
  Node = list_to_atom(capricorn_config:get("cluster", "node", "cluster")),
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
  
  capricorn_config:register(fun
  ("cluster", "node") -> ?MODULE:stop()
  end, self()),
  
  InstalledGems = ets:new(gems, [set,private,{keypos,2}]),
  
  {ok, #ctx{
    knows_cluster  = KnowsCluster,
    cluster        = Node,
    installed_gems = InstalledGems
  }}.
 
%% @spec handle_call(_Request, _From, State) -> {reply, Reply, State}
%% @doc Callback for synchronous requests
handle_call({ensure_gems_are_present_for_app, App}, _From, State) ->
  R = it_ensure_gem_for_app(App, State),
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

it_ensure_gem_for_app(App, Ctx) ->
  it_ensure_gem_for_app(App#application.required_gems, App, Ctx).
it_ensure_gem_for_app([], App, #ctx{}) -> {ok, App};
it_ensure_gem_for_app([Gem|Rest], App, #ctx{cluster=Cluster}=Ctx) ->
  case capricorn_cluster_gems:lookup(Cluster, Gem) of
  {ok, Spec} -> 
    
    ?LOG_DEBUG("found gem ~p", [Gem]),
    case it_is_gem_installed(Spec, Ctx) of
    true  ->
      ?LOG_DEBUG("gem already installed ~p", [Gem]),
      Deps = [Dep || Dep <- Spec#gem.deps],
      it_install_gems(Deps, Ctx),
      it_ensure_gem_for_app(Rest, App#application{
        installed_gems=[Spec#gem.id|App#application.installed_gems]
      }, Ctx);
      
    false ->
      ?LOG_DEBUG("installing gem ~p", [Gem]),
      case it_install_gem(Spec, Ctx) of
      {ok, Spec1} ->
        ?LOG_DEBUG("installed gem ~p", [Gem]),
        it_ensure_gem_for_app(Rest, App#application{
          installed_gems=[Spec1#gem.id|App#application.installed_gems]
        }, Ctx);
        
      {error, E} ->
        ?LOG_DEBUG("error ~p", [E]),
        {error, E}
      end
    end;
    
  {error, not_found} -> 
    ?LOG_DEBUG("error ~p", [{missing_gem, Gem}]),
    {error, {missing_gem, Gem}}
  end.

it_install_gem(#gem{deps=Deps}=Spec, #ctx{cluster=Cluster}=Ctx) ->
  case it_install_gems(Deps, Ctx) of
  ok ->
    ?LOG_DEBUG("pull gem ~s", [(Spec#gem.id)#gem_id.name]),
    case capricorn_cluster_gems:pull(Cluster, Spec) of
    {ok, Data} ->
      ?LOG_DEBUG("staging gem ~s", [(Spec#gem.id)#gem_id.name]),
      case file:write_file("/tmp/capricorn-gem.gem", Data) of
      ok ->
        ?LOG_INFO("installing gem ~s", [(Spec#gem.id)#gem_id.name]),
        case gem_exec("install --no-rdoc --no-ri --local --no-update-sources /tmp/capricorn-gem.gem") of
        "Successfully"++_ -> {ok, Spec};
        Error ->
          ?LOG_DEBUG("error ~p", [Error]),
          {error, {install_failed, (Spec#gem.id)#gem_id.name}}
        end;
      Error -> 
        ?LOG_DEBUG("error ~p", [Error]),
        Error
      end;
    {error, E} -> 
      ?LOG_DEBUG("error ~p", [E]),
      {error, E}
    end;
  {error, E} ->
    ?LOG_DEBUG("error ~p", [E]),
    {error, E}
  end.

it_install_gems([], _Ctx) -> ok;
it_install_gems([Dep|Rest], #ctx{cluster=Cluster}=Ctx) ->
  case capricorn_cluster_gems:lookup(Cluster, Dep) of
  {ok, Spec} ->
    ?LOG_DEBUG("found gem ~s", [Dep#dependency.name]),
    case it_is_gem_installed(Spec, Ctx) of
    true  ->
      ?LOG_DEBUG("allready installed gem ~s", [Dep#dependency.name]),
      Deps = [Dep || Dep <- Spec#gem.deps],
      it_install_gems(Deps++Rest, Ctx);
    false ->
      case it_install_gem(Spec, Ctx) of
      {ok, _Spec1} ->
        it_install_gems(Rest, Ctx);
      {error, E} -> 
        ?LOG_DEBUG("error ~p", [E]),
        {error, E}
      end
    end;
  {error, not_found} -> {error, {missing_gem, Dep}}
  end.

it_find_deep_deps(#gem{deps=Deps}=Spec, Acc1, Ctx) ->
  Acc3 = lists:foldl(fun(Dep, Acc2) ->
    it_find_deep_deps(Dep, Acc2, Ctx)
  end, Acc1, Deps),
  [Spec|Acc3];
it_find_deep_deps(#dependency{}=Dep, Acc1, #ctx{cluster=Cluster}=Ctx) ->
  case capricorn_cluster_gems:lookup(Cluster, Dep) of
  {ok, Spec} -> it_find_deep_deps(Spec, Acc1, Ctx);
  {error, not_found} -> {error, {missing_gem, Dep}}
  end.

it_is_gem_installed(#gem{}=Gem, #ctx{installed_gems=T}) ->
  case ets:lookup(T, Gem#gem.id) of
  [] ->
    Args = "list -i",
    case (Gem#gem.id)#gem_id.name of
    undefined -> % skip
      {error, badarg};
    Name ->
      Args1 = lists:concat([Args, "  ", binary_to_list(Name)]),
      Args2 =
      case (Gem#gem.id)#gem_id.version of
      undefined -> Args1;
      Version   -> lists:concat([Args1, " -v \"",
        capricorn_cluster_gems:version_to_string(Version), "\""])
      end,
      case gem_exec(Args2) of
      "true\n" ->
        ets:insert_new(T,Gem),
        true;
      _Else -> false
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
