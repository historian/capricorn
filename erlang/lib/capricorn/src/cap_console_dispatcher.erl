-module(cap_console_dispatcher).
-export([start_link/1, stop/0]).
-include("capricorn.hrl").

% start misultin http server
start_link(Port) ->
  misultin:start_link([
    {port, Port},
    {loop,    fun(Req) -> handle_http(Req, Port) end},
    {ws_loop, fun(Ws)  -> handle_websocket(Ws)   end}
  ]).

% stop misultin
stop() ->
  misultin:stop().

% callback on request received
handle_http(Req, _Port) ->
  handle_rest(Req:get(method), Req:resource([lowercase, urldecode]), Req).


handle_rest('GET', [], Req) ->
  Req:ok("Hello World.");

handle_rest(Method, ["machines" |Rest], Req) ->
  handle_machines_rest(Method, Rest, Req);

handle_rest(Method, ["gems" |Rest], Req) ->
  handle_gems_rest(Method, Rest, Req);

handle_rest(_, _, Req) ->
  handle_404(Req).


handle_gems_rest('GET', [], Req) ->
  {ok, All} = cap_cluster_gems:all(cap_config:get(machine, cluster, cluster)),
  JSON = lists:foldl(fun(Gem, Acc) ->
    [gem_id_to_json(Gem) |Acc]
  end, [], All),
  Req:ok([{"Content-Type", "application/json"}], ejson:encode(JSON));

handle_gems_rest('POST', [], Req) ->
  Req:ok([{"Content-Type", "text/plain"}], "handle_gems_rest: POST, []");

handle_gems_rest(Method, [Gem, Version |Rest], Req) ->
  handle_gem_rest(Method, {Gem, Version}, Rest, Req);

handle_gems_rest(_, _, Req) ->
  handle_404(Req).


handle_gem_rest('GET', {Gem, Version}, [], Req) ->
  Req:ok([{"Content-Type", "text/plain"}], "handle_gem_rest: GET, ~p, []", [{Gem, Version}]);

handle_gem_rest('PUT', {Gem, Version}, [], Req) ->
  Req:ok([{"Content-Type", "text/plain"}], "handle_gem_rest: PUT, ~p, []", [{Gem, Version}]);

handle_gem_rest('DELETE', {Gem, Version}, [], Req) ->
  Req:ok([{"Content-Type", "text/plain"}], "handle_gem_rest: DELETE, ~p, []", [{Gem, Version}]);

handle_gem_rest(_, _, _, Req) ->
  handle_404(Req).


handle_machines_rest('GET', [], Req) ->
  Nodes    = [atom_to_list(node()) | [atom_to_list(Node)   || Node <- nodes()]],
  Machines = [list_to_binary(Node) || Node <- Nodes,
               string:substr(Node, 1, 8) == "machine-"],
  Req:ok([{"Content-Type", "application/json"}], ejson:encode(Machines));

handle_machines_rest(Method, [Machine |Rest], Req) ->
  handle_machine_rest(Method, Machine, Rest, Req);

handle_machines_rest(_, _, Req) ->
  handle_404(Req).


handle_machine_rest(Method, Machine, ["applications" |Rest], Req) ->
  handle_apps_rest(Method, Machine, Rest, Req);

handle_machine_rest(_, _, _, Req) ->
  handle_404(Req).


handle_apps_rest('GET', Machine, [], Req) ->
  All  = cap_machine_apps:all(list_to_atom(Machine)),
  JSON = lists:foldl(fun(App, Acc) ->
    [app_to_json(App) |Acc]
  end, [], All),
  Req:ok([{"Content-Type", "application/json"}], ejson:encode(JSON));

handle_apps_rest('POST', Machine, [], Req) ->
  Req:ok([{"Content-Type", "text/plain"}], "handle_apps_rest: POST, ~p, []", [Machine]);

handle_apps_rest(Method, Machine, [Application |Rest], Req) ->
  handle_app_rest(Method, Machine, Application, Rest, Req);

handle_apps_rest(_, _, _, Req) ->
  handle_404(Req).


handle_app_rest('GET', Machine, Application, [], Req) ->
  Resp = cap_machine_apps:one(list_to_atom(Machine), ?l2b(Application)),
  case Resp of
  {ok, App} ->
    JSON = ejson:encode(app_to_json(App)),
    Req:ok([{"Content-Type", "application/json"}], JSON);
  _ ->
    handle_404(Req)
  end;

handle_app_rest('PUT', Machine, Application, [], Req) ->
  Req:ok([{"Content-Type", "text/plain"}], "handle_apps_rest: PUT, ~p, ~p, []", [Machine, Application]);

handle_app_rest('DELETE', Machine, Application, [], Req) ->
  Req:ok([{"Content-Type", "text/plain"}], "handle_apps_rest: DELETE, ~p, ~p, []", [Machine, Application]);

handle_app_rest('POST', Machine, Application, ["start"], Req) ->
  Req:ok([{"Content-Type", "text/plain"}], "handle_apps_rest: POST, ~p, ~p, [start]", [Machine, Application]);

handle_app_rest('POST', Machine, Application, ["restart"], Req) ->
  Req:ok([{"Content-Type", "text/plain"}], "handle_apps_rest: POST, ~p, ~p, [restart]", [Machine, Application]);

handle_app_rest('POST', Machine, Application, ["stop"], Req) ->
  Req:ok([{"Content-Type", "text/plain"}], "handle_apps_rest: POST, ~p, ~p, [stop]", [Machine, Application]);

handle_app_rest('POST', Machine, Application, ["update"], Req) ->
  Req:ok([{"Content-Type", "text/plain"}], "handle_apps_rest: POST, ~p, ~p, [update]", [Machine, Application]);

handle_app_rest(_, _, _, _, Req) ->
  handle_404(Req).


handle_404(Req) ->
  Req:respond(404, [{"Content-Type", "text/plain"}], "Page not found.").


% callback on received websockets data
handle_websocket(Ws) ->
  receive
    {browser, Data} ->
      Ws:send(["received '", Data, "'"]),
      handle_websocket(Ws);
    _Ignore ->
      handle_websocket(Ws)
  after 5000 ->
    Ws:send("pushing!"),
    handle_websocket(Ws)
  end.





app_to_json(App) ->
  % -record(application, {
  %   id=undefined            :: 'undefined' | binary(),
  %   node=undefined          :: 'undefined' | atom(),
  %   name=undefined          :: 'undefined' | binary(),
  %   domains=[]              :: [binary(),...],
  %   environment=development :: atom(),
  %   www_user=undefined      :: 'undefined' | binary(),
  %   www_group=undefined     :: 'undefined' | binary(),
  %   root_path=undefined     :: 'undefined' | binary(),
  %   installed_gems=[]       :: [gem_id()],
  %   required_gems=[]        :: [binary()],
  %   rvsn={rvsn, 0}
  % }).
    
  {[
    {<<"id">>,             App#application.id},
    {<<"node">>,           ?a2b(App#application.node)},
    {<<"name">>,           App#application.name},
    {<<"domains">>,        App#application.domains},
    {<<"environment">>,    ?a2b(App#application.environment)},
    {<<"www_user">>,       App#application.www_user},
    {<<"www_group">>,      App#application.www_group},
    {<<"root_path">>,      App#application.root_path},
    {<<"installed_gems">>, lists:foldl(fun(Gem, Acc1) ->
        [gem_id_to_json(Gem) |Acc1]
      end, [], App#application.required_gems)},
    {<<"required_gems">>,  App#application.required_gems}
  ]}.


gem_id_to_json(Gem) ->
  % -record(gem_id, {
  %   name    :: binary(),
  %   version :: version()
  % }).
  % -type version_part()  :: pos_integer() | string() .
  % -type version_parts() :: [version_part(),...] .
  % -type version()       :: {version_parts()} .
  
  {Parts} = Gem#gem_id.version,
  Version = lists:foldl(fun(Part, Acc2) ->
    Str = case Part of
    Part when is_integer(Part) ->
      integer_to_list(Part);
    Part ->
      Part
    end,
    
    case Acc2 of
    undefined -> Str;
    _ -> Acc2 ++ [$. |Str]
    end
  end, undefined, Parts),
  
  {[
    {<<"name">>,    Gem#gem_id.name},
    {<<"version">>, ?l2b(Version)}
  ]}.

