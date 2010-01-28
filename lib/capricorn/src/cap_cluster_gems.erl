-module(cap_cluster_gems).
-behaviour(gen_server).
-include("capricorn.hrl").

-export([start_link/0, push/1, missing/0, lookup/1, lookup/2, pull/1, pull/2, check/0, all/0]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([version_to_string/1]).

-record(ctx, {table, gem_path, stage_path, spec_reader, tmp_id=0}).

push({ok, Data}) ->
  push(Data);
push(Path) when is_list(Path) ->
  push(file:read_file(Path));
push(Data) ->
  gen_server:call(cap_cluster_gems, {push, Data}).

lookup(GemName) ->
  gen_server:call(cap_cluster_gems, {lookup, GemName}).

lookup(Node, GemName) ->
  gen_server:call({cap_cluster_gems, Node}, {lookup, GemName}).

pull(Spec) ->
  gen_server:call(cap_cluster_gems, {pull, Spec}).

pull(Node, Spec) ->
  gen_server:call({cap_cluster_gems, Node}, {pull, Spec}).

missing() ->
  gen_server:call(cap_cluster_gems, {missing}).

all() ->
  gen_server:call(cap_cluster_gems, {all}).

check() ->
  gen_server:cast(cap_cluster_gems, check).

start_link() ->
  gen_server:start_link({local, cap_cluster_gems}, ?MODULE, [], []).

init([]) ->
  Root = cap_config:get("cluster", "database", "var/run/capricorn"),
  
  TablePath = filename:join([Root, "gems.db"]),
  {ok, Ref} = dets:open_file(cap_cluster_gems, [{file, TablePath}, {keypos, 2}]),
  update_gems_table(Ref),
  
  GemPath = filename:join([Root, "gems"]),
  os:cmd("mkdir -p "++GemPath),
  
  StagePath = filename:join([Root, "stage"]),
  os:cmd("mkdir -p "++StagePath),
  
  State  = #ctx{table=Ref, gem_path=GemPath, stage_path=StagePath},
  State1 = start_spec_reader(State),
  
  {ok, State1}.


handle_call({push, Data}, _From, #ctx{stage_path=Stage,tmp_id=TmpId}=State) ->
  StageGemPath = filename:join([Stage, integer_to_list(TmpId) ++ ".gem"]),
  R = file:write_file(StageGemPath, Data),
  ?LOG_INFO("G: ~p ~p", [StageGemPath, R]),
  try it_push(StageGemPath, State) of
  {ok, Missing} -> {reply, {ok, Missing}, State#ctx{tmp_id=TmpId+1}}
  catch
  Error ->
    file:delete(StageGemPath),
    {reply, Error, State#ctx{tmp_id=TmpId+1}}
  end;

handle_call({pull, #gem{id=Id}}, _From, State) ->
  handle_call({pull, Id}, _From, State);

handle_call({pull, #gem_id{}=Id}, _From, State) ->
  Path = gem_path(Id, State),
  {reply, file:read_file(Path), State};

handle_call({missing}, _From, #ctx{table=T}=State) ->
  Missing = dets:foldl(fun(#gem{missing=Deps}, Acc)->
    Acc++Deps
  end, [], T),
  {reply, {ok, lists:usort(Missing)}, State};

handle_call({all}, _From, #ctx{table=T}=State) ->
  All = dets:foldl(fun(Gem, Acc)->
    [Gem#gem.id |Acc]
  end, [], T),
  {reply, {ok, lists:usort(All)}, State};

handle_call({lookup, #dependency{}=Dep}, _From, State) ->
  case it_find_last_consistent_gem(Dep, State) of
  not_found -> {reply, {error, not_found}, State};
  GemSpec   -> {reply, {ok,    GemSpec},   State}
  end;

handle_call({lookup, GemName}, _From, State) ->
  case it_find_last_consistent_gem(GemName, State) of
  not_found -> {reply, {error, not_found}, State};
  GemSpec   -> {reply, {ok,    GemSpec},   State}
  end;

handle_call(Msg, _From, State) ->
  ?LOG_DEBUG("Unhandled call: ~p", [Msg]),
  {reply, ok, State}.


handle_cast(check, State) ->
  it_check(State),
  {noreply, State};

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(Msg, State) ->
  ?LOG_DEBUG("Unhandled cast: ~p", [Msg]),
  {noreply, State}.


handle_info({Port, {exit_status, _Status}}, #ctx{spec_reader=Port}=State) ->
  {noreply, start_spec_reader(State)};

handle_info(Info, State) ->
  ?LOG_DEBUG("Unhandled info: ~p", [Info]),
  {noreply, State}.


terminate(_Reason, #ctx{table=T,spec_reader=P}) ->
  dets:close(T),
  bertio:port_close(P, stop),
  ok.


code_change(_OldVsn, State, _Extra) ->
  {ok, State}.


start_spec_reader(State) ->
  RubyCmd        = os:find_executable("ruby"),
  SpecReaderArgs = {args, [filename:join([code:priv_dir(capricorn), "internal/bin/capricorn_gem_spec.rb"])]},
  SpecReader     = bertio:open_port({spawn_executable, RubyCmd}, [exit_status, SpecReaderArgs]),
  State#ctx{spec_reader=SpecReader}.

it_find_last_consistent_gem(#dependency{reqs=Reqs,name=Name}, #ctx{table=T}) ->
  dets:foldl(fun
  (#gem{missing=[]}=Spec, #gem{}=Acc) ->
    if      ((Spec#gem.id)#gem_id.name == Name)
    andalso ((Spec#gem.id)#gem_id.version > (Acc#gem.id)#gem_id.version) ->
      case match_requirements((Spec#gem.id)#gem_id.version, Reqs) of
      true -> Spec;
      false -> Acc
      end;
    true ->
      Acc
    end;
  (#gem{missing=[]}=Spec, not_found) ->
    if ((Spec#gem.id)#gem_id.name == Name) ->
      case match_requirements((Spec#gem.id)#gem_id.version, Reqs) of
      true -> Spec;
      false -> not_found
      end;
    true ->
      not_found
    end;
  (#gem{missing=_}, Acc) -> Acc
  end, not_found, T);

it_find_last_consistent_gem(Gem, #ctx{table=T}) ->
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

it_find_last_gem(Gem, #ctx{table=T}) ->
  dets:foldl(fun
  (#gem{}=Spec, #gem{}=Acc) ->
    if      ((Spec#gem.id)#gem_id.name == Gem)
    andalso ((Spec#gem.id)#gem_id.version > (Acc#gem.id)#gem_id.version) ->
      Spec;
    true ->
      Acc
    end
  end, not_found, T).

it_push(StageGemPath, #ctx{table=T}=Ctx) ->
  case it_spec(StageGemPath, Ctx) of
  #gem{id=Id,deps=Deps}=Spec ->
    case dets:member(T,Id) of
    false ->
      [_, Missing] = find_dependencies(Deps, Ctx),
      Spec1 = Spec#gem{missing=Missing},
      case dets:insert_new(T, Spec1) of
      true ->
        GemPath = gem_path(Spec1, Ctx),
        file:rename(StageGemPath, GemPath),
        mark_gem_as_found(Id, Missing, Ctx),
        {ok, Missing};
      false -> throw({error, already_present})
      end;
    _ -> throw({error, already_present})
    end;
  E -> throw(E)
  end.

it_spec(GemPath, Ctx) when is_list(GemPath) ->
  it_spec(list_to_binary(GemPath),Ctx);
it_spec(GemPath, #ctx{spec_reader=P}) ->
  bertio:send(P, GemPath),
  try bertio:recv(P) of
    {bert, {gem, _, _, _}=Gem} -> normalize_gem(Gem);
    {bert, R} -> R
  catch
    error:timeout -> {error, timeout}
  end.

it_check(#ctx{table=T}=Ctx) ->
  dets:foldl(fun(Spec, Acc) ->
    mark_gem_as_found(Spec, [], Ctx),
    Acc
  end, ok, T).

mark_gem_as_found(#gem{id=Id}, NewMissing, Ctx) ->
  mark_gem_as_found(Id, NewMissing, Ctx);
mark_gem_as_found(#gem_id{}=Id, NewMissing, #ctx{table=T}) ->
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
  [#dependency{reqs=Reqs, name=Name}=Dep|Rest], Acc, Update) ->
  case match_requirements(Version, Reqs) of
  true  -> fold_missing_dependants(Id, Spec, Rest, Acc, true);
  false -> fold_missing_dependants(Id, Spec, Rest, [Dep|Acc], Update)
  end;
fold_missing_dependants(Id, Spec, [Dep|Rest], Acc, Update) ->
  fold_missing_dependants(Id, Spec, Rest, [Dep|Acc], Update).

gem_path(#gem{id=Id}, Ctx) ->
  gem_path(Id, Ctx);
gem_path(#gem_id{name=Name, version=Version}, #ctx{gem_path=GemPath}) ->
  filename:join([GemPath, binary_to_list(Name) ++ "-" ++version_to_string(Version) ++ ".gem"]).

version_to_string(#version{parts=Parts}) ->
  case version_parts_to_string(Parts, "") of
  "." ++ V -> V;
  R -> R
  end.

version_parts_to_string([], Acc) -> Acc;
version_parts_to_string([Part|Rest], Acc) when is_integer(Part) ->
  version_parts_to_string(Rest, Acc++"."++integer_to_list(Part));
version_parts_to_string([Part|Rest], Acc) ->
  version_parts_to_string(Rest, Acc++"."++Part).

find_dependencies(Deps, Ctx) ->
  find_dependencies(Deps, [], [], Ctx).

find_dependencies([], Found, Missing, _Ctx) -> [Found, lists:usort(Missing)];
find_dependencies([Dep|Rest], Found, Missing, Ctx) ->
  case find_dependency(Dep, Ctx) of
  undefined -> 
    find_dependencies(Rest, Found, [Dep|Missing], Ctx);
  #gem{missing=M}=Spec ->
    find_dependencies(Rest, [Spec|Found], M++Missing, Ctx)
  end.

find_dependency(#dependency{name=Name, reqs=Reqs}, #ctx{table=T}) ->
  dets:foldl(fun(#gem{id=#gem_id{name=Name1, version=Version}}=Spec, Acc) ->
    if Name1 == Name ->
      case match_requirements(Version, Reqs) of
      true  ->
        case Acc of
        #gem{id=#gem_id{version=Version1}} ->
          case match_requirement(Version1, #requirement{op=(<<">">>), version=Version}) of
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

balance_versions([#version{parts=A},#version{parts=B}]) ->
  [C,D] = balance_version_parts([A,B]),
  [#version{parts=C},#version{parts=D}].
balance_version_parts([A,B]) ->
  if
  size(A) > size(B) ->
    case lists:nth(size(B)+1, A) of
    E when is_list(E) ->
      balance_versions([A, B++[[255]]]);
    _ ->
      balance_versions([A, B++[0]])
    end;
  size(A) < size(B) -> 
    case lists:nth(size(A)+1, B) of
    E when is_list(E) ->
      balance_versions([A++[[255]], B]);
    _ ->
      balance_versions([A++[0], B])
    end;
  size(A) == size(B) ->
    [A,B];
  true ->
    [A,B]
  end.

release_version(#version{parts=Parts}) ->
  #version{parts=release_version(Parts, [])}.
release_version([], Acc) ->
  lists:reverse(Acc);
release_version([Part|_], Acc) when is_list(Part) ->
  lists:reverse(Acc);
release_version([Part|Rest], Acc) when is_integer(Part) ->
  release_version(Rest, [Part|Acc]).

bump_version(#version{}=Version) ->
  #version{parts=Parts1} = release_version(Version),
  Parts2 = if
    length(Parts1) > 1 ->
      lists:sublist(Parts1, length(Parts1)-1);
    true -> Parts1
  end,
  [Last|Rest] = lists:reverse(Parts2),
  Parts3 = lists:reverse([Last+1|Rest]),
  #version{parts=Parts3}.

match_requirements(_, []) -> true;
match_requirements(Version, [Req|Rest]) ->
  case match_requirement(Version, Req) of
  true  -> match_requirements(Version, Rest);
  false -> false
  end.

match_requirement(#version{}=Version, #requirement{op=(<<"=">>), version=RVersion}) ->
  [Version1, RVersion1] = balance_versions([Version, RVersion]),
  Version1 == RVersion1;
match_requirement(#version{}=Version, #requirement{op=(<<"!=">>), version=RVersion}) ->
  [Version1, RVersion1] = balance_versions([Version, RVersion]),
  Version1 /= RVersion1;
match_requirement(#version{}=Version, #requirement{op=(<<"<">>), version=RVersion}) ->
  [Version1, RVersion1] = balance_versions([Version, RVersion]),
  Version1 < RVersion1;
match_requirement(#version{}=Version, #requirement{op=(<<">">>), version=RVersion}) ->
  [Version1, RVersion1] = balance_versions([Version, RVersion]),
  Version1 > RVersion1;
match_requirement(#version{}=Version, #requirement{op=(<<">=">>), version=RVersion}) ->
  [Version1, RVersion1] = balance_versions([Version, RVersion]),
  Version1 >= RVersion1;
match_requirement(#version{}=Version, #requirement{op=(<<"<=">>), version=RVersion}) ->
  [Version1, RVersion1] = balance_versions([Version, RVersion]),
  Version1 =< RVersion1;
match_requirement(#version{}=Version, #requirement{op=(<<"~>">>), version=RVersion}) ->
  Version1  = release_version(Version),
  RVersion1 = bump_version(RVersion),
  match_requirement(Version1, #requirement{op=(<<">=">>), version=RVersion}) and 
  match_requirement(Version1, #requirement{op=(<<"<">>), version=RVersion1}).


normalize_gem({gem, Name, Version, Deps}) ->
  #gem{
    id=#gem_id{
      name=Name,
      version=normalize_gem_version(Version)},
    deps=normalize_gem_dependencies(Deps,[])}.

normalize_gem_version(Version) when is_binary(Version) ->
  normalize_gem_version(binary_to_list(Version)); 
normalize_gem_version(Version) ->
  Parts = string:tokens(Version, "."),
  #version{parts=normalize_gem_version_parts(Parts, [])}.

normalize_gem_version_parts([], Acc) ->
  lists:reverse(Acc);
normalize_gem_version_parts([Part|Rest], Acc) ->
  case string:to_integer(Part) of
  {I,[]} -> normalize_gem_version_parts(Rest, [I|Acc]);
  _      -> normalize_gem_version_parts(Rest, [Part|Acc])
  end.

normalize_gem_dependencies([], Acc) ->
  lists:reverse(Acc);
normalize_gem_dependencies([{dependency,Name,Reqs}|Rest], Acc) ->
  Dep = #dependency{name=Name, reqs=normalize_gem_requirements(Reqs, [])},
  normalize_gem_dependencies(Rest, [Dep|Acc]).

normalize_gem_requirements([], Acc) ->
  lists:reverse(Acc);
normalize_gem_requirements([{requirement, Op, Version}|Rest], Acc) ->
  Req = #requirement{op=Op, version=normalize_gem_version(Version)},
  normalize_gem_requirements(Rest, [Req|Acc]).


update_gems_table(Table) ->
  cap_dets_updater:update(Table, fun
  ({gem, _Id, _Deps, _Missing, {rvsn, 0}}) ->
    ok;
  ({gem, Id, Deps, Missing}) ->
    {update, {gem, Id, Deps, Missing, {rvsn, 0}}}
  end).

