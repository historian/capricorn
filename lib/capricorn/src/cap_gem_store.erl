-module(cap_gem_store).
-include("capricorn.hrl").
-behaviour(gen_server).


-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).


%%% Start the server
start_link(Options) ->
  gen_server:start_link({local, cap_gem_store}, ?MODULE, Options, []).


%%% Initialize the server
init(Options) ->
  Graph    = digraph:new([acyclic, private]),
  Versions = ets:new([bag, private]),
  Specs    = ets:new([set, private]),
  Missing  = ets:new([bag, private]),
  
  {ok, {Deps,Versions,Specs,Missing}}.


%%% Handle call messages
handle_call({add, #gem{}=Spec}, _From, State) ->
  
  {reply, ok, State}.
handle_call(_Request, _From, State) ->
  {reply, ok, State}.


%%% Handle cast messages
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


%% Internal API
do_add(Name, Version, Deps, {Graph,Versions,Specs,Missing}) ->
  case ets:lookup(Specs, {Name, Version}) of
  [_] ->
    {error, {already_present, Name, Version}};
  [] ->
    % store vertex
    Vertex = digraph:add_vertex(Graph),
    
    % resolve dependencies
    lists:foreach(fun({DepName, Reqs}) ->
      
      PotentialMatches = ets:lookup_element(Versions, DepName, 2),
      case cap_gem_utils:find_matches(PotentialMatches, Reqs) of
      [] ->  % if missing
        ets:insert_new(Missing, {DepName, Reqs, {Name, Version}});
        
      Matches ->
        lists:foreach(fun(MatchVersion) ->
          [MatchVertex] = ets:lookup_element(Specs, {DepName, MatchVersion}, 3),
          Edge = digraph:add_edge(Graph, Vertex, MatchVersion)
        end, Matches),
      end
      
    end, Deps),
    
    % resolve dependants
    case ets:lookup_element(Versions, Name, 2) of
    [] -> ignore;
    [OlderVersion|_] ->
      [OlderVertex] = ets:lookup_element(Specs, {Name, OlderVersion}, 3),
      DependantVertices = digraph:in_neighbours(Graph, OlderVertex),
      [digraph:add_edge(Graph, DependantVertex, Vertex) || DependantVertex <- DependantVertices]
    end,
    
    % resolve dependants with missing
    FoundDeps = ets:lookup(Missing, Name),
    lists:foreach(fun({_, Reqs, {DepName, DepVersion}}=MissingSpec) ->
      case cap_gem_utils:match_requirements(Version, Reqs) of
      true  -> 
        [DepVertex] = ets:lookup_element(Specs, {DepName, DepVersion}, 3),
        digraph:add_edge(Graph, DepVertex, Vertex),
        ets:delete_object(Missing, MissingSpec);
      false -> ignore
      end
    end, FoundDeps),
    
    % store spec
    ets:insert(Versions, {Name, Version}),
    ets:insert_new(Specs, {{Name, Version}, Deps, Vertex}),
  end


do_lookup()

