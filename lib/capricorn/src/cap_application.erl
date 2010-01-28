-module(cap_application).
-include("capricorn.hrl").
-behaviour(gen_server).

-export([start_link/1]).
-export([restart/1, relink/1, update/1, stop/1, start/1, service/2, service/4]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%%% Start the server
start_link(#application{}=App) ->
  {ok, Appname} = get_proc_def(App),
  gen_server:start_link(Appname, ?MODULE, [App], []).

restart(App) ->
  {ok, Appname} = get_proc_name(App),
  gen_server:cast(Appname, {restart}).

update(App) ->
  {ok, Appname} = get_proc_name(App),
  gen_server:call(Appname, {update, App}).

relink(App) ->
  {ok, Appname} = get_proc_name(App),
  gen_server:cast(Appname, {relink}).

stop(App) ->
  {ok, Appname} = get_proc_name(App),
  gen_server:cast(Appname, {stop}).

start(App) ->
  {ok, Appname} = get_proc_name(App),
  gen_server:cast(Appname, {start}).

service(App, [M,F,A]) ->
  service(App, M,F,A).

service(App, M,F,A) ->
  {ok, Appname} = get_proc_name(App),
  gen_server:call(Appname, {service, M,F,A}, 6000).
  

get_proc_def(App) ->
  case get_proc_name(App) of
  {ok, L} when is_atom(L) -> {ok, {local, L}};
  Else -> Else
  end.
get_proc_name(#application{id=undefined}) ->
  {error, no_master_domain};
get_proc_name(#application{id=Id, node=Node}) ->
  get_proc_name({Node, Id});
get_proc_name({Node, Id}) ->
  if Node == node() ->
    {ok, list_to_atom(lists:flatten([binary_to_list(Id), ".app"]))};
  true ->
    {ok, {list_to_atom(lists:flatten([binary_to_list(Id), ".app"])), Node}}
  end.

-record(state, {app=undefined,runner=undefined,restarts=0}).

%%% Initialize the server
init([#application{}=App]) ->
  
  State  = #state{app=App},
  State1 = restart_runner(State),
  
  {ok, State1}.
 
%%% Handle call messages
handle_call({service, M,F,A}, _From, #state{runner=R}=State) ->
  bertio:send(R, [M,F,A]),
  try bertio:recv(R, 5000) of
  {bert, Result} -> {reply, Result, State}
  catch
    error:timeout -> {reply, timeout, State}
  end;

handle_call({update, App}, _From, State) ->
  ?LOG_DEBUG("reconfiguring app ~s", [App#application.id]),
  case reconfigure_app(App) of
  ok ->
    ?LOG_DEBUG("reconfigured app ~s", [App#application.id]),
    os:cmd("touch "++get_app_root(App, 'host/tmp/relink.txt')),
    os:cmd("touch "++get_app_root(App, 'host/tmp/restart.txt')),
    app_chown(App, 'host/tmp/relink.txt'),
    app_chown(App, 'host/tmp/restart.txt'),
    {reply, ok, restart_runner(State#state{restarts=0,app=App})};
  Error ->
    ?LOG_ERROR("Error while relinking app with new gems: ~p", [Error]),
    {reply, ok, State}
  end;

handle_call(_Request, _From, State) ->
  {reply, ok, State}.


%%% Handle cast messages
handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast({restart}, #state{app=App}=State) ->
  os:cmd("touch "++get_app_root(App, 'host/tmp/restart.txt')),
  app_chown(App, 'host/tmp/restart.txt'),
  {noreply, restart_runner(State#state{restarts=0})};

handle_cast({relink}, #state{app=App}=State) ->
  os:cmd("touch "++get_app_root(App, 'host/tmp/relink.txt')),
  os:cmd("touch "++get_app_root(App, 'host/tmp/restart.txt')),
  app_chown(App, 'host/tmp/relink.txt'),
  app_chown(App, 'host/tmp/restart.txt'),
  {noreply, restart_runner(State#state{restarts=0})};

handle_cast({stop}, #state{app=App}=State) ->
  os:cmd("touch "++get_app_root(App, 'host/tmp/stop.txt')),
  app_chown(App, 'host/tmp/stop.txt'),
  {noreply, State};

handle_cast({start}, #state{app=App}=State) ->
  os:cmd("rm -f "++get_app_root(App, 'host/tmp/stop.txt')),
  {noreply, State};

handle_cast(Msg, State) ->
  ?LOG_DEBUG("unhandled cast ~p", [Msg]),
  {noreply, State}.


%%% Handle generic messages
handle_info({Port, {exit_status, _Status}}, #state{runner=Port,restarts=3}=State) ->
  {noreply, State#state{runner=undefined,restarts=0}};
handle_info({Port, {exit_status, _Status}}, #state{runner=Port}=State) ->
  {noreply, restart_runner(State)};

handle_info(_Info, State) ->
  {noreply, State}.

%%% Before stopping the server
terminate(_Reason, _State) ->
  ok.

%%% Code Changes
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


restart_runner(#state{runner=undefined, restarts=Restarts,
  app=#application{root_path=Root, environment=Environment, www_user=User}}=State) ->
  AppRoot = filename:join([binary_to_list(Root), "host"]),
  Ruby  = os:find_executable("ruby"),
  Args = {args, [
    filename:join([code:priv_dir(capricorn), "internal/bin/capricorn_app_runner.rb"]),
    atom_to_list(Environment),
    binary_to_list(User)
  ]},
  Port = bertio:open_port({spawn_executable, Ruby}, [exit_status, {cd, AppRoot}, Args]),
  State#state{runner=Port, restarts=Restarts + 1};
restart_runner(#state{runner=Port}=State) when is_port(Port) ->
  catch bertio:port_close(Port),
  restart_runner(State#state{runner=undefined}).

reconfigure_app(App) ->
  write_milkshake_gem_config(App).

write_milkshake_gem_config(#application{installed_gems=[]})        -> {ok, "gems: {}\n"};
write_milkshake_gem_config(#application{installed_gems=Gems}=App)  ->
  Header = "gems:\n",
  {ok, Config} = write_milkshake_gem_config(Header, Gems),
  ?LOG_DEBUG("config: ~s", [Config]),
  ?LOG_DEBUG("writing milkshake.yml to ~s", [get_app_root(App, 'host/config/milkshake.yml')]),
  file:write_file(get_app_root(App, 'host/config/milkshake.yml'), Config),
  app_chown(App, 'host/config/milkshake.yml').

write_milkshake_gem_config(Config, []) -> {ok, Config};
write_milkshake_gem_config(Config, [#gem_id{name=Name,version=Version}|Other]) ->
  case Name of
  undefined -> % skip
    write_milkshake_gem_config(Config, Other);
  _Else1 ->
    Config1 = lists:concat([Config, "  ", binary_to_list(Name), ":\n"]),
    Config2 =
    case Version of
    undefined -> lists:concat([Config1, "    version: \">= 0\"\n"]);
    _Else2    -> lists:concat([Config1, "    version: \"", cap_cluster_gems:version_to_string(Version), "\"\n"])
    end,
    write_milkshake_gem_config(Config2, Other)
  end.

get_app_root(App, Sub) when is_atom(Sub) ->
  get_app_root(App, [atom_to_list(Sub)]);
get_app_root(#application{root_path=Path}, Sub) ->
  filename:join([binary_to_list(Path)|Sub]).


app_chown(#application{www_user=U,www_group=G}=App, Path) ->
  os:cmd("chown "++binary_to_list(U)++":"++binary_to_list(G)++" "++get_app_root(App, Path)),
  ok.
