-module(cap_application).
-include("capricorn.hrl").
-behaviour(gen_server).

-export([start_link/1]).
-export([restart/1, relink/1, update/1, stop/1, start/1]).
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
  gen_server:call(Appname, {update, App}, 15 * 60000).

relink(App) ->
  {ok, Appname} = get_proc_name(App),
  gen_server:cast(Appname, {relink}).

stop(App) ->
  {ok, Appname} = get_proc_name(App),
  gen_server:cast(Appname, {stop}).

start(App) ->
  {ok, Appname} = get_proc_name(App),
  gen_server:cast(Appname, {start}).


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

-record(state, {app=undefined}).

%%% Initialize the server
init([#application{}=App]) ->

  State  = #state{app=App},

  gen_server:cast(self(), {restart}),

  {ok, State}.

%%% Handle call messages
handle_call({update, App}, _From, State) ->
  ?LOG_DEBUG("reconfiguring app ~s", [App#application.id]),
  case reconfigure_app(App) of
  ok ->
    ?LOG_DEBUG("reconfigured app ~s", [App#application.id]),
    gen_server:cast(self(), {relink}),
    {reply, ok, State#state{app=App}};
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
  {noreply, State};

handle_cast({relink}, #state{app=App}=State) ->
  os:cmd("touch "++get_app_root(App, 'host/tmp/relink.txt')),
  os:cmd("touch "++get_app_root(App, 'host/tmp/restart.txt')),
  app_chown(App, 'host/tmp/relink.txt'),
  app_chown(App, 'host/tmp/restart.txt'),
  {noreply, State};

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
handle_info(_Info, State) ->
  {noreply, State}.

%%% Before stopping the server
terminate(_Reason, _State) ->
  ok.

%%% Code Changes
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


reconfigure_app(App) ->
  write_milkshake_gem_config(App).

write_milkshake_gem_config(#application{installed_gems=[]})        -> {ok, "gems: {}\n"};
write_milkshake_gem_config(App) ->
  Header = "gems:\n",
  Gems = select_required_installed_gems(App),
  {ok, Config} = write_milkshake_gem_config(Header, Gems),
  ?LOG_DEBUG("config: ~s", [Config]),
  ?LOG_DEBUG("writing milkshake.yml to ~s", [get_app_root(App, 'host/config/milkshake.yml')]),
  file:write_file(get_app_root(App, 'host/config/milkshake.yml'), Config),
  app_chown(App, 'host/config/milkshake.yml').

write_milkshake_gem_config(Config, []) -> {ok, Config};
write_milkshake_gem_config(Config, [#gem_id{}=Id|Other]) ->
  #gem_id{name=Name,version=Version} = Id,
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

select_required_installed_gems(App) ->
  #application{installed_gems=Installed, required_gems=Required} = App,
  lists:foldl(fun
  (#gem_id{name=Name}=Id, Acc) ->
    case lists:member(Name, Required) of
    true  -> Acc ++ [Id];
    false -> Acc
    end
  end, [], Installed).

get_app_root(App, Sub) when is_atom(Sub) ->
  get_app_root(App, [atom_to_list(Sub)]);
get_app_root(#application{root_path=Path}, Sub) ->
  filename:join([binary_to_list(Path)|Sub]).


app_chown(#application{www_user=U,www_group=G}=App, Path) ->
  os:cmd("chown "++binary_to_list(U)++":"++binary_to_list(G)++" "++get_app_root(App, Path)),
  ok.
