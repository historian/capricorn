-module(ep2p_contact_list).
-behaviour(gen_server).

-export ([start_link/0, lookup/1, publish/3]).

-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

lookup(Name) ->
  gen_server:call(?MODULE, {lookup, Name}).

publish(Name, Addresses, Cookie) ->
  gen_server:call(?MODULE, {publish, Name, Addresses, Cookie}).

init([]) ->
  {ok, dict:new()}.

handle_call({lookup, Name}, _From, Nodes) ->
  case dict:find(Name, Nodes) of
  {ok, {Addresses, Cookie, _}} -> {reply, {ok, Addresses, Cookie}, Nodes};
  _                            -> {reply, {error, not_found},      Nodes}
  end;

handle_call({publish, Name, Addresses, Cookie}, _From, Nodes) ->
  Creation = case dict:find(Name, Nodes) of
  {ok, {_, _, Creation1}} -> Creation1 + 1;
  _ -> 1
  end,

  Nodes1 = dict:store(Name, {Addresses, Cookie, Creation}, Nodes),
  handle_cast({request_sync}, Nodes1),
  {reply, {ok, Creation}, Nodes1};

handle_call({bootstrap, Name, Addresses, Cookie}, _From, Nodes) ->
  Creation = 0,
  Nodes1 = dict:store(Name, {Addresses, Cookie, Creation}, Nodes),
  {reply, {ok, Creation}, Nodes1};

handle_call({to_list}, _From, Nodes) ->
  NodeList = dict:to_list(Nodes),
  {reply, {ok, NodeList}, Nodes}.

handle_cast({request_sync}, Nodes) ->
  [begin
    gen_server:cast({?MODULE, Node}, {sync_rec, node()})
  end || Node <- nodes()],
  {noreply, Nodes};

handle_cast({sync_req, Node}, Nodes) ->
  case gen_server:call({?MODULE, Node}, {to_list}) of
  {ok, NodeList} ->
    {Nodes1, Updated} =
    lists:foldl(fun({Name, {_, _, Date2}=V2}, {Acc, Updated}) ->
      case dict:find(Name, Acc) of
      {ok, {_, _, Date1}} when Date1 < Date2 ->
        {dict:store(Name, V2, Acc), true};
      _ ->
        {Acc, Updated}
      end
    end, {Nodes, false}, NodeList),

    case Updated of
    true  -> handle_cast({request_sync}, Nodes1);
    false -> {noreply, Nodes1}
    end;
  _ -> {noreply, Nodes}
  end.

handle_info(_, Nodes) ->
  {noreply, Nodes}.

terminate(_, _Nodes) ->
  ok.

code_change(_, Nodes, _) ->
  {ok, Nodes}.