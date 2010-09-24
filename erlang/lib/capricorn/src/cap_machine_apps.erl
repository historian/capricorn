-module(cap_machine_apps).
-include("capricorn.hrl").
-behaviour(gen_server).



-export([start_link/0]).
-export([create/4, create/5, import/7, import/8, update/3, update/4, fupdate/1, fupdate/2, update_gem/1, update_gem/2, all/0, all/1, one/1, one/2]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).



-record(ctx, {recipe :: binary(),apps,scaffolder}).



%%% Start the server
start_link() ->
  gen_server:start_link({local, cap_machine_apps}, ?MODULE, [], []).



-spec create(binary(),[binary(),...],atom(),[binary()]) -> ok.

create(Name, Domains, Environment, Gems) ->
  gen_server:cast(cap_machine_apps, {create, Name, Domains, Environment, Gems}).



-spec create(atom(), binary(),[binary(),...],atom(),[binary()]) -> ok.

create(Node, Name, Domains, Environment, Gems) ->
  gen_server:cast({cap_machine_apps, Node}, {create, Name, Domains, Environment, Gems}).



-spec import(binary(),[binary(),...],atom(),[binary()], binary(), binary(), binary()) -> 'ok'.

import(Name, Domains, Environment, Gems, Root, Uid, Gid) ->
  gen_server:cast(cap_machine_apps, {import, Name, Domains, Environment, Gems, Root, Uid, Gid}).



-spec import(atom(), binary(),[binary(),...],atom(),[binary()], binary(), binary(), binary()) -> 'ok'.

import(Node, Name, Domains, Environment, Gems, Root, Uid, Gid) ->
  gen_server:cast({cap_machine_apps, Node}, {import, Name, Domains, Environment, Gems, Root, Uid, Gid}).



-spec update(binary(), [binary(),...], [binary()]) -> ok.

update(Id, Domains, Gems) ->
  gen_server:cast(cap_machine_apps, {update, Id, Domains, Gems}).



-spec update(atom(), binary(), [binary(),...], [binary()]) -> ok.

update(Node, Id, Domains, Gems) ->
  gen_server:cast({cap_machine_apps, Node}, {update, Id, Domains, Gems}).



-spec fupdate(binary()) -> ok.

fupdate(Id) ->
  gen_server:cast(cap_machine_apps, {fupdate, Id}).



-spec fupdate(atom(), binary()) -> ok.

fupdate(Node, Id) ->
  gen_server:cast({cap_machine_apps, Node}, {fupdate, Id}).



-spec update_gem(binary()) -> ok.

update_gem(Name) ->
  gen_server:cast(cap_machine_apps, {update_gem, Name}).



-spec update_gem(atom(), binary()) -> ok.

update_gem(Node, Name) ->
  gen_server:cast({cap_machine_apps, Node}, {update_gem, Name}).



-spec all() -> [application()].

all() ->
  gen_server:call(cap_machine_apps, {all}).



-spec all(atom()) -> [application()].

all(Node) ->
  gen_server:call({cap_machine_apps, Node}, {all}).



-spec one(binary()) -> application().

one(Id) ->
  gen_server:call(cap_machine_apps, {one, Id}).



-spec one(atom(), binary()) -> application().

one(Node, Id) ->
  gen_server:call({cap_machine_apps, Node}, {one, Id}).



%%% Initialize the server
init([]) ->
  Root   = cap_config:get(machine, database, "var/run/capricorn"),
  Recipe = cap_config:get(machine, recipe, "macports"),

  TablePath = filename:join([Root, "applications.db"]),
  {ok, Ref} = dets:open_file(cap_machine_apps, [{file, TablePath}, {keypos, 2}]),
  update_apps_table(Ref),

  State  = #ctx{recipe=list_to_binary(Recipe), apps=Ref},
  State1 = start_scaffolder(State),

  {ok, State1}.



%%% Handle call messages
handle_call({all}, _From, #ctx{apps=Apps}=State) ->
  All = dets:foldl(fun(App, Acc) -> [App|Acc] end, [], Apps),
  {reply, All, State};

handle_call({one, Id}, _From, #ctx{}=State) ->
  try
    {ok, App} = do_lookup_app(Id, State),
    {reply, {ok, App}, State}
  catch
    T:E ->
      ?LOG_ERROR("error while getting one app ~s: ~p", [Id, E]),
      {reply, {T, E}, State}
  end;

handle_call(_Request, _From, State) ->
  {reply, ok, State}.



%%% Handle cast messages
handle_cast({create, Name, [MainDomain|_]=Domains, Environment, Gems}, Ctx) ->
  App = #application{
    id=MainDomain,
    node=node(),
    name=Name,
    domains=Domains,
    environment=Environment,
    required_gems=Gems},
  case valid_app(App) of
  true ->
    R = do_create(App, Ctx),
    ?LOG_ERROR("app: ~p", [R]),
    {noreply, Ctx};
  {false, E} ->
    ?LOG_ERROR("Invalid app: ~p", [E]),
    {noreply, Ctx}
  end;

handle_cast({import, Name, [MainDomain|_]=Domains, Environment, Gems, Root, Uid, Gid}, Ctx) ->
  App = #application{
    id=MainDomain,
    node=node(),
    name=Name,
    domains=Domains,
    environment=Environment,
    www_user=Uid,
    www_group=Gid,
    root_path=Root,
    required_gems=Gems},
  case valid_app(App) of
  true ->
    R = do_import(App, Ctx),
    ?LOG_ERROR("app: ~p", [R]),
    {noreply, Ctx};
  {false, E} ->
    ?LOG_ERROR("Invalid app: ~p", [E]),
    {noreply, Ctx}
  end;

handle_cast({update, Id, Domains, Gems}, Ctx) ->
  do_update(Id, Domains, Gems, Ctx),
  {noreply, Ctx};

handle_cast({fupdate, Id}, Ctx) ->
  do_fupdate(Id, Ctx),
  {noreply, Ctx};

handle_cast({update_gem, GemName}, Ctx) ->
  #ctx{ apps=Apps } = Ctx,

  dets:foldl(fun
  (App, _) ->
    Included = lists:foldl(fun
    (#gem_id{name = Name}, _) when Name == GemName ->
      true;
    (_, Acc) ->
      Acc
    end, false, App#application.installed_gems),

    case Included of
    true ->
      do_fupdate(App#application.id, Ctx);
    false ->
      ignore
    end
  end, [], Apps),
  {noreply, Ctx};

handle_cast(stop, Ctx) ->
  {stop, normal, Ctx};

handle_cast(_Msg, State) ->
  {noreply, State}.


%%% Handle generic messages
handle_info({Port, {exit_status, _Status}}, #ctx{scaffolder=Port}=State) ->
  {noreply, start_scaffolder(State)};

handle_info(_Info, State) ->
  {noreply, State}.



%%% Before stopping the server
terminate(_Reason, #ctx{apps=Apps}) ->
  dets:close(Apps),
  ok.



%%% Code Changes
code_change(_OldVsn, State, _Extra) ->
  {ok, State}.



start_scaffolder(State) ->
  Cmd  = os:find_executable("capricornutl"),
  Port = bertio:open_port({spawn_executable, Cmd}, [
    exit_status, {args, ["internal:scaffolder"]}]),
  State#ctx{scaffolder=Port}.



-spec do_update(binary(),[binary(),...],[binary(),...],#ctx{}) -> {'error',_}|{'ok',application()}.

do_update(Id, Domains, Gems, Ctx) ->
  ?LOG_INFO("updating ~s => ~p", [Id, {Domains, Gems}]),
  try
    {ok, App1} = do_lookup_app(Id, Ctx),
    {ok, App2} = do_update_domains(App1, Domains, Ctx),
    {ok, App3} = do_try_update_gems(App2, Gems,Ctx),
    {ok, App4} = do_save_app(App3, Ctx),
    {ok, App4}
  catch
    error:E ->
      ?LOG_ERROR("error while updating app ~s: ~p", [Id, E]),
      {error, E}
  end.



-spec do_fupdate(binary(),#ctx{}) -> {'error',_}|{'ok',application()}.

do_fupdate(Id, Ctx) ->
  ?LOG_INFO("queued update for ~s", [Id]),
  emq:push(machine_queue, {fupdate, Id}, fun() ->
    ?LOG_INFO("force updating ~s", [Id]),
    try
      {ok, App1} = do_lookup_app(Id, Ctx),
      {ok, App2} = do_update_gems(App1, App1#application.required_gems, Ctx),
      {ok, App3} = do_save_app(App2, Ctx),
      {ok, App3}
    catch
      error:E ->
        ?LOG_ERROR("error while updating app ~s: ~p", [Id, E]),
        {error, E}
    end
  end).



-spec do_lookup_app(binary(), #ctx{}) -> {ok, application()}.

do_lookup_app(Id, #ctx{apps=Apps}) ->
  case dets:lookup(Apps, Id) of
  [] -> throw(not_found);
  [App] -> {ok, App}
  end.



-spec do_save_app(application(), #ctx{}) -> {ok, application()}.

do_save_app(App, #ctx{apps=Apps}) ->
  dets:insert(Apps, App),
  {ok, App}.



-spec do_update_domains(application(), [binary(),...], #ctx{}) -> {ok, application()}.

do_update_domains(App, Domains, #ctx{recipe=Recipe, scaffolder=P}) ->
  if Domains /= App#application.domains ->
    bertio:send(P, {update, Recipe, App}),
    receive
    {P, {bert, true}} -> {ok, App#application{domains=Domains}};
    {P, {bert, E}} -> throw(E)
    end;
  true -> {ok, App}
  end.



-spec do_try_update_gems(application(), [binary(),...], #ctx{}) -> {ok, application()}.

do_try_update_gems(App, Gems, Ctx) ->
  if Gems /= App#application.required_gems ->
    do_update_gems(App, Gems, Ctx);
  true ->
    ?LOG_DEBUG("app gems are already updated", []),
    {ok, App}
  end.



-spec do_update_gems(application(), [binary(),...], #ctx{}) -> {ok, application()}.

do_update_gems(App, Gems, _Ctx) ->
  ?LOG_INFO("updating app gems ~p => ~p", [App#application.required_gems, Gems]),

  App1 = App#application{required_gems=Gems,installed_gems=[]},

  case cap_machine:ensure_gems_are_present_for_app(App1) of
  {ok, App2} ->
    ?LOG_INFO("relinking app ~s", [App#application.id]),
    case cap_application:update(App2) of
    ok ->
      ?LOG_DEBUG("relinked app ~s", [App#application.id]),
      {ok, App2};
    Error ->
      ?LOG_DEBUG("error ~p", [Error]),
      Error
    end;
  Error ->
    ?LOG_DEBUG("error ~p", [Error]),
    Error
  end.



-spec do_create(application(), #ctx{}) -> ok | any().

do_create(App, #ctx{recipe=Recipe, apps=Apps, scaffolder=P}) ->
  bertio:send(P, {create, Recipe, App}),
  try bertio:recv(P, 25000) of
  {bert, {true, {User, Group, RootPath}}} ->
    App1 = App#application{www_user=User,www_group=Group,root_path=RootPath},
    dets:insert_new(Apps, App1),
    cap_machine_apps_sup:start(App1),
    ok;
  {bert, R} -> R
  catch
    error:timeout -> {error, timeout}
  end.



-spec do_import(application(), #ctx{}) -> ok | any().

do_import(App, #ctx{apps=Apps}) ->
  dets:insert_new(Apps, App),
  cap_machine_apps_sup:start(App),
  ok.



-spec valid_app(application()) -> 'true' | {'false',binary()}.

valid_app(#application{node=No,name=N,domains=D,required_gems=G}) ->
  if
  (not is_atom(No)) orelse No /= node() ->
    {false, <<"Invalid Node">>};
  (not is_binary(N) orelse size(N) == 0) ->
    {false, <<"Invalid Name">>};
  (not is_list(D) orelse length(D) == 0) ->
    {false, <<"Please pass at least one domain">>};
  (not is_list(G) orelse length(G) == 0) ->
    {false, <<"Please pass at least one gem">>};
  true ->
    true
  end.



update_apps_table(Table) ->
  cap_dets_updater:update(Table, fun

  ({application, _Id, _Node, _Name, _Domains, _Environment, _User,
    _Group, _Root, _Installed, _Required, {rvsn, 1}}) ->
    ok;

  ({application, Id, Node, Name, Domains, Environment, User,
    Group, Root, Installed, Required, {rvsn, 0}}) ->
    GemIds = [lists:nth(2, erlang:tuple_to_list(Gem)) || Gem <- Installed],
    {update, {application, Id, Node, Name, Domains, Environment,
      User, Group, Root, GemIds, Required, {rvsn, 1}}};

  ({application, Id, Node, Name, Domains, Environment, User,
    Group, Root, Installed, Required}) ->
    {update, {application, Id, Node, Name, Domains, Environment,
      User, Group, Root, Installed, Required, {rvsn, 0}}}

  end).


