-module(hatter_config_srv).
-behaviour(gen_server).



start_link(ConfigStore) ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, {ConfigStore}, []).


init({ConfigStore}) ->


handle_call(Msg, From, State) ->
handle_cast(Msg, State) ->
handle_info(Msg, State) ->


handle_request(Request, Socket) ->
handle_websocket(Request, Socket) ->

terminate(Reason, State) ->
code_change(OldVsn, State, Extra) ->
