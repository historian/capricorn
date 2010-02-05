-module(bertrpc_hello_world).
-behaviour(bertrpc_connection).


-export([listen_link/1, connect_link/2, hello/2, greet/2, how/2, howl/1]).
-export([init/1, handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

-export([test/0]).


hello(Pid, Name) ->
  bertrpc_connection:call(Pid, hello_world, hello, [Name]).
greet(Pid, Name) ->
  bertrpc_connection:call(Pid, hello_world, greet, [Name]).
how(Pid, Name) ->
  gen_server:cast(Pid, {greet, Name}).
howl(Pid) ->
  bertrpc_connection:cast(Pid, wolf, howl, []).


%%% Start the server
listen_link(Port) ->
  bertrpc_connection:listen_link({local, bertrpc_hello_world}, ?MODULE, [], Port).

connect_link(Host, Port) ->
  bertrpc_connection:connect_link(Host, Port).


%%% Initialize the server
init([]) ->
  State = [],
  {ok, State}.


%%% Handle call messages
handle_call({hello_world, hello, [Name], _}, _From, State) ->
  {reply, <<"Hello ", Name/binary>>, State};

handle_call({hello_world, greet, [Who], _}=Request, From, _) ->
  Self = self(),
  io:format("call: -> ~p\n", [Request]),
  spawn_link(fun() -> catch receive after 10 ->
    io:format("how\n", []),
    bertrpc_hello_world:how(Self, <<"Hi ">>)
  end end),
  io:format("call: <- ~p\n", [{Who, From}]),
  {noreply, {Who, From}};

handle_call(Request, _From, State) ->
  io:format("call: ~p\n", [Request]),
  {reply, ok, State}.


%%% Handle cast messages
handle_cast({wolf, howl, [], _}, State) ->
  io:format("Ohh , the werewolf, the werewolf...\n"),
  {noreply, State};

handle_cast({greet, How}=Request, {Who, From}) ->
  io:format("cast: -> ~p\n", [Request]),
  bertrpc_connection:reply(From, <<How/binary, Who/binary>>),
  io:format("cast: <- ~p\n", [{noreply, []}]),
  {noreply, []};

handle_cast(stop, State) ->
  {stop, normal, State};

handle_cast(Msg, State) ->
  io:format("cast: ~p\n", [Msg]),
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


test() ->
  bertrpc_hello_world:listen_link(5000),
  
  {ok, C1}  = bertrpc_hello_world:connect_link("localhost", 5000),
  {ok, C2}  = bertrpc_hello_world:connect_link("localhost", 5000),
  
  io:format("C1: ~p\n", [bertrpc_hello_world:hello(C1, <<"world!">>)]),
  io:format("C2: ~p\n", [bertrpc_hello_world:hello(C2, <<"simon!">>)]),
  io:format("C1: ~p\n", [bertrpc_hello_world:hello(C1, <<"moon!">> )]),
  io:format("C2: ~p\n", [bertrpc_hello_world:hello(C2, <<"anais!">>)]),
  
  io:format("C1: ~p\n", [bertrpc_hello_world:greet(C1, <<"World!">>)]),
  io:format("C2: ~p\n", [bertrpc_hello_world:greet(C2, <<"Moon!">>)]),
  
  io:format("C1: ~p\n", [bertrpc_hello_world:howl(C1)]),
  io:format("C2: ~p\n", [bertrpc_hello_world:howl(C2)]),
  
  erlang:halt().

