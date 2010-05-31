-module(cap_cluster_gems).
-behaviour(gen_server).
-include("capricorn.hrl").

-export([start_link/0, push/1, missing/0, lookup/1, lookup/2, pull/1, pull/2, check/0, all/0]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([version_to_string/1]).

-record(state, {table, gem_path, stage_path, spec_reader, tmp_id=0}).
-type state() :: #state{} .


-spec push(binary() | string()) -> {ok, [dependency()]} | {error, term()} .
push(Path) when is_list(Path) ->
  case file:read_file(Path) of
  {ok, Data} -> push(Data);
  Else       -> Else
  end;
push(Data) ->
  gen_server:call(cap_cluster_gems, {push, Data}).


-spec lookup(binary() | dependency()) -> {ok, gem_spec()} | {error, not_found} .
lookup(GemName) ->
  gen_server:call(cap_cluster_gems, {lookup, GemName}).


-spec lookup(node(), binary() | dependency()) -> {ok, gem_spec()} | {error, not_found} .
lookup(Node, GemName) ->
  gen_server:call({cap_cluster_gems, Node}, {lookup, GemName}).


-spec pull(gem_id() | gem_spec()) -> {ok, binary()} | {error, term()} .
pull(Spec) ->
  gen_server:call(cap_cluster_gems, {pull, Spec}).


-spec pull(node(), gem_id() | gem_spec()) -> {ok, binary()} | {error, term()} .
pull(Node, Spec) ->
  gen_server:call({cap_cluster_gems, Node}, {pull, Spec}).


-spec missing() -> {ok, [dependency()]} .
missing() ->
  gen_server:call(cap_cluster_gems, {missing}).


-spec all() -> {ok, [gem_id()]} .
all() ->
  gen_server:call(cap_cluster_gems, {all}).


-spec check() -> ok .
check() ->
  gen_server:cast(cap_cluster_gems, check).


-spec start_link() -> {ok, pid()} .
start_link() ->
  gen_server:start_link({local, cap_cluster_gems}, ?MODULE, [], []).


init([]) ->
  Root = cap_config:get(cluster, database, "var/run/capricorn"),

  TablePath = filename:join([Root, "gems.db"]),
  {ok, Ref} = dets:open_file(cap_cluster_gems, [{file, TablePath}, {keypos, 2}]),
  update_gems_table(Ref),

  GemPath = filename:join([Root, "gems"]),
  os:cmd("mkdir -p "++GemPath),

  StagePath = filename:join([Root, "stage"]),
  os:cmd("mkdir -p "++StagePath),

  State  = #state{table=Ref, gem_path=GemPath, stage_path=StagePath},
  State1 = start_spec_reader(State),

  {ok, State1}.


handle_call({push, Data}, _From, #state{stage_path=Stage,tmp_id=TmpId}=State) ->
  StageGemPath = filename:join([Stage, integer_to_list(TmpId) ++ ".gem"]),
  R = file:write_file(StageGemPath, Data),
  ?LOG_INFO("G: ~p ~p", [StageGemPath, R]),
  case do_push(StageGemPath, State) of
  {ok, Missing} ->
    {reply, {ok, Missing}, State#state{tmp_id=TmpId+1}};
  {error, Reason} ->
    file:delete(StageGemPath),
    {reply, {error, Reason}, State#state{tmp_id=TmpId+1}}
  end;

handle_call({pull, #gem{id=Id}}, _From, State) ->
  handle_call({pull, Id}, _From, State);

handle_call({pull, #gem_id{}=Id}, _From, State) ->
  Path = gem_path(Id, State),
  {reply, file:read_file(Path), State};

handle_call({missing}, _From, #state{table=T}=State) ->
  Missing = dets:foldl(fun(#gem{missing=Deps}, Acc)->
    Acc++Deps
  end, [], T),
  {reply, {ok, lists:usort(Missing)}, State};

handle_call({all}, _From, #state{table=T}=State) ->
  All = dets:foldl(fun(Gem, Acc)->
    [Gem#gem.id |Acc]
  end, [], T),
  {reply, {ok, lists:usort(All)}, State};

handle_call({lookup, {_,_}=Dep}, _From, State) ->
  case do_find_last_consistent_gem(Dep, State) of
  not_found -> {reply, {error, not_found}, State};
  GemSpec   -> {reply, {ok,    GemSpec},   State}
  end;

handle_call({lookup, GemName}, _From, State) ->
  case do_find_last_consistent_gem(GemName, State) of
  not_found -> {reply, {error, not_found}, State};
  GemSpec   -> {reply, {ok,    GemSpec},   State}
  end;

handle_call(Msg, _From, State) ->
  ?LOG_DEBUG("Unhandled call: ~p", [Msg]),
  {reply, ok, State}.


handle_cast(check, State) ->
  do_check(State),
  {noreply, State};

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(Msg, State) ->
  ?LOG_DEBUG("Unhandled cast: ~p", [Msg]),
  {noreply, State}.


handle_info({Port, {exit_status, _Status}}, #state{spec_reader=Port}=State) ->
  {noreply, start_spec_reader(State)};

handle_info(Info, State) ->
  ?LOG_DEBUG("Unhandled info: ~p", [Info]),
  {noreply, State}.


terminate(_Reason, #state{table=T,spec_reader=P}) ->
  dets:close(T),
  bertio:port_close(P),
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


-spec start_spec_reader(state()) -> state() .
start_spec_reader(State) ->
  RubyCmd    = os:find_executable("capricorn-gem-spec"),
  SpecReader = bertio:open_port({spawn_executable, RubyCmd}, [exit_status]),
  State#state{spec_reader=SpecReader}.


-spec do_find_last_consistent_gem(dependency() | binary(), state()) -> not_found | gem_spec() .
do_find_last_consistent_gem({Name, Reqs}, #state{table=T}) ->
  dets:foldl(fun
  (#gem{missing=[]}=Spec, #gem{}=Acc) ->
    if      ((Spec#gem.id)#gem_id.name == Name)
    andalso ((Spec#gem.id)#gem_id.version > (Acc#gem.id)#gem_id.version) ->
      case cap_gem_utils:match_requirements((Spec#gem.id)#gem_id.version, Reqs) of
      true -> Spec;
      false -> Acc
      end;
    true ->
      Acc
    end;
  (#gem{missing=[]}=Spec, not_found) ->
    if ((Spec#gem.id)#gem_id.name == Name) ->
      case cap_gem_utils:match_requirements((Spec#gem.id)#gem_id.version, Reqs) of
      true -> Spec;
      false -> not_found
      end;
    true ->
      not_found
    end;
  (#gem{missing=_}, Acc) -> Acc
  end, not_found, T);

do_find_last_consistent_gem(Gem, #state{table=T}) ->
  dets:foldl(fun
  (#gem{missing=[]}=Spec, #gem{}=Acc) ->
    if      ((Spec#gem.id)#gem_id.name == Gem)
    andalso ((Spec#gem.id)#gem_id.version > (Acc#gem.id)#gem_id.version) ->
      Spec;
    true ->
      Acc
    end;
  (#gem{missing=[]}=Spec, not_found) ->
    if ((Spec#gem.id)#gem_id.name == Gem) ->
      Spec;
    true ->
      not_found
    end;
  (#gem{missing=_}, Acc) -> Acc
  end, not_found, T).


-spec do_push(string() | binary(), state()) -> {ok, [dependency()]} |  {error, already_present} | {error, timeout | {not_found} | {gem_error, binary()}} .
do_push(StageGemPath, #state{table=T}=Ctx) ->
  case do_spec(StageGemPath, Ctx) of
  #gem{id=Id,deps=Deps}=Spec ->
    case dets:member(T,Id) of
    false ->
      {_, Missing} = find_dependencies(Deps, Ctx),
      Spec1 = Spec#gem{missing=Missing},
      case dets:insert_new(T, Spec1) of
      true ->
        GemPath = gem_path(Spec1, Ctx),
        file:rename(StageGemPath, GemPath),
        mark_gem_as_found(Id, Missing, Ctx),
        case Missing of
        [] ->
          % GemName = Id#gem_id.name,
          % [cap_machine_apps:update_gem(Node, GemName) || Node <- nodes()],
          {ok, Missing};
        Missing ->
          {ok, Missing}
        end;
      false -> {error, already_present}
      end;
    _ -> {error, already_present}
    end;
  E -> E
  end.


-spec do_spec(string() | binary(), state()) -> gem_spec() | {error, timeout | {not_found} | {gem_error, binary()}} .
do_spec(GemPath, Ctx) when is_list(GemPath) ->
  do_spec(list_to_binary(GemPath),Ctx);
do_spec(GemPath, #state{spec_reader=P}) ->
  bertio:send(P, GemPath),
  try bertio:recv(P) of
    {bert, {_, _, _}=Gem} -> normalize_gem(Gem);
    {bert, R} -> R
  catch
    error:timeout -> {error, timeout}
  end.

-spec do_check(state()) -> ok .
do_check(#state{table=T}=Ctx) ->
  dets:foldl(fun(Spec, Acc) ->
    mark_gem_as_found(Spec, [], Ctx),
    Acc
  end, ok, T).

mark_gem_as_found(#gem{id=Id}, NewMissing, Ctx) ->
  mark_gem_as_found(Id, NewMissing, Ctx);
mark_gem_as_found(#gem_id{}=Id, NewMissing, #state{table=T}) ->
  UpdatedSpecs = dets:foldl(fun(#gem{missing=Missing}=Spec, Acc) ->
    case fold_missing_dependants(Id, Spec, Missing, NewMissing, false) of
    {true, Spec1} -> [Spec1|Acc];
    {false,   _} -> Acc
    end
  end, [], T),
  dets:insert(T, UpdatedSpecs).

fold_missing_dependants(#gem_id{}, Spec, [], Acc, Update) ->
  {Update, Spec#gem{missing=Acc}};
fold_missing_dependants(#gem_id{name=Name, version=Version}=Id, Spec,
  [{Name, Reqs}=Dep|Rest], Acc, Update) ->
  case cap_gem_utils:match_requirements(Version, Reqs) of
  true  -> fold_missing_dependants(Id, Spec, Rest, Acc, true);
  false -> fold_missing_dependants(Id, Spec, Rest, [Dep|Acc], Update)
  end;
fold_missing_dependants(Id, Spec, [Dep|Rest], Acc, Update) ->
  fold_missing_dependants(Id, Spec, Rest, [Dep|Acc], Update).


-spec gem_path(gem_spec() | gem_id(), state()) -> string() .
gem_path(#gem{id=Id}, Ctx) ->
  gem_path(Id, Ctx);
gem_path(#gem_id{name=Name, version=Version}, #state{gem_path=GemPath}) ->
  filename:join([GemPath, binary_to_list(Name) ++ "-" ++version_to_string(Version) ++ ".gem"]).


-spec version_to_string(version()) -> string().
version_to_string({Parts}) ->
  case version_parts_to_string(Parts, "") of
  "." ++ V -> V;
  R -> R
  end.

-spec version_parts_to_string(version_parts(), string()) -> string().
version_parts_to_string([], Acc) -> Acc;
version_parts_to_string([Part|Rest], Acc) when is_integer(Part) ->
  version_parts_to_string(Rest, Acc++"."++integer_to_list(Part));
version_parts_to_string([Part|Rest], Acc) ->
  version_parts_to_string(Rest, Acc++"."++Part).


-spec find_dependencies([dependency()], state()) -> {[gem_spec()], [dependency()]} .
find_dependencies(Deps, Ctx) ->
  find_dependencies(Deps, [], [], Ctx).


-spec find_dependencies([dependency()], [gem_spec()], [dependency()], state()) -> {[gem_spec()], [dependency()]} .
find_dependencies([], Found, Missing, _Ctx) -> {Found, lists:usort(Missing)};
find_dependencies([Dep|Rest], Found, Missing, Ctx) ->
  case find_dependency(Dep, Ctx) of
  undefined ->
    find_dependencies(Rest, Found, [Dep|Missing], Ctx);
  #gem{missing=M}=Spec ->
    find_dependencies(Rest, [Spec|Found], M++Missing, Ctx)
  end.


-spec find_dependency(dependency(), state()) -> undefined | gem_spec() .
find_dependency({Name, Reqs}, #state{table=T}) ->
  dets:foldl(fun(#gem{id=#gem_id{name=Name1, version=Version}}=Spec, Acc) ->
    if Name1 == Name ->
      case cap_gem_utils:match_requirements(Version, Reqs) of
      true  ->
        case Acc of
        #gem{id=#gem_id{version=Version1}} ->
          case cap_gem_utils:match_requirement(Version1, {'>', Version}) of
          true  -> Acc;
          false -> Spec
          end;
        undefined -> Spec
        end;
      false -> Acc
      end;
    true ->
      Acc
    end
  end, undefined, T).


-type port_requirement() :: {binary(), binary()} .
-type port_dependency()  :: {binary(), [port_requirement()]} .
-type port_gem_spec()    :: {binary(), binary(), [port_dependency()]} .

-spec normalize_gem(port_gem_spec()) -> gem_spec().
normalize_gem({Name, Version, Deps1}) ->
  Deps2 = [begin
    {DepName,Reqs1} = Dep1,
    Reqs2 = [begin
      {Op, Version1} = Req1,
      Version2 = normalize_gem_version(Version1),
      {?b2a(Op), Version2}
    end || Req1 <- Reqs1],
    {DepName, Reqs2}
  end || Dep1 <- Deps1],

  #gem{
    id=#gem_id{
      name=Name,
      version=normalize_gem_version(Version)},
    deps=Deps2}.


-spec normalize_gem_version(binary() | string() | version()) -> version() .
normalize_gem_version({Parts}) when is_list(Parts) ->
  {Parts};
normalize_gem_version(Version) when is_binary(Version) ->
  normalize_gem_version(binary_to_list(Version));
normalize_gem_version(Version) ->
  {[case string:to_integer(Part) of
    {I,[]} when is_integer(I) -> I;
    _Else -> Part
  end || Part <- string:tokens(Version, ".")]}.


update_gems_table(Table) ->
  cap_dets_updater:update(Table, fun
  ({gem, _Id, _Deps, _Missing, {rvsn, 2}}) ->
    ok;

  ({gem, Id1, Deps1, Missing1, {rvsn, 1}}) ->
    UpVersion = fun
      ({{Parts}}) -> {Parts};
      ({Parts}) when is_list(Parts) -> {Parts}
    end,
    UpReq     = fun({Op, V})  -> {Op, UpVersion(V)}        end,
    UpDep     = fun({Name, Reqs1}) ->
      Reqs2 = [UpReq(Req) || Req <- Reqs1],
      {Name, Reqs2}
    end,

    Deps2    = [UpDep(Dep) || Dep <- Deps1],
    Missing2 = [UpDep(Mis) || Mis <- Missing1],

    {update, {gem, Id1, Deps2, Missing2, {rvsn, 2}}};

  ({gem, Id1, Deps1, Missing1, {rvsn, 0}}) ->
    UpVersion = fun
      ({version, Parts}) -> {Parts};
      ({Parts}) -> {Parts}
    end,
    UpId      = fun({gem_id, Name, V})     -> {gem_id, Name, UpVersion(V)}    end,
    UpReq     = fun({requirement, Op, V})  -> {?b2a(Op), UpVersion(V)}        end,
    UpDep     = fun({dependency, Name, Reqs1}) ->
      Reqs2 = [UpReq(Req) || Req <- Reqs1],
      {Name, Reqs2}
    end,

    Id2      = UpId(Id1),
    Deps2    = [UpDep(Dep) || Dep <- Deps1],
    Missing2 = [UpDep(Mis) || Mis <- Missing1],

    {update, {gem, Id2, Deps2, Missing2, {rvsn, 1}}};

  ({gem, Id, Deps, Missing}) ->
    {update, {gem, Id, Deps, Missing, {rvsn, 0}}}

  end).

