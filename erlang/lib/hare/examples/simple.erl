-module(simple).
% -behaviour(hare_server).

-export([start_link/0, init/1, handle_transaction/2]).

start_link() ->
  hare_server:start_link({local, ?MODULE}, ?MODULE, [{port, 4000}]).

init(_Conn) ->
  % io:format("new connection\n", []),
  {ok, state}.

% handle_transaction(T0, _State) ->
%
%   T1 = T0:respond('200', [
%     {'Content-Type', 'text/html'},
%     {'Content-Length', '473'}]),
%
%   Out = T1:get(response, stream),
%
%   Out:send("<h1>Hello World</h1>"),
%
%   Out:send("<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"),
%
%   {ok, _State}.



handle_transaction(T0, _State) ->

  T1 = T0:respond('200', [
    {'Content-Type', 'text/html'},
    {'Transfer-Encoding', 'chunked'}]),

  Out = T1:get(response, stream),

  Out:send(<<"<h1>Hello World</h1>">>),

  Out:send(<<"<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>">>),

  Out:send(""),

  {ok, _State}.



% handle_transaction(T0, _State) ->
%
%   T1 = T0:respond('200', [
%     {'Content-Type', 'text/html'},
%     {'Transfer-Encoding', 'chunked'}]),
%
%   Out = T1:get(response, stream),
%
%   Out:send("<h1>Hello World</h1>"),
%
%   Out:send("<p>Lorem ipsum dolor sit amet, consectetur adipisicing elit, sed do eiusmod tempor incididunt ut labore et dolore magna aliqua. Ut enim ad minim veniam, quis nostrud exercitation ullamco laboris nisi ut aliquip ex ea commodo consequat. Duis aute irure dolor in reprehenderit in voluptate velit esse cillum dolore eu fugiat nulla pariatur. Excepteur sint occaecat cupidatat non proident, sunt in culpa qui officia deserunt mollit anim id est laborum.</p>"),
%
%   Out:send(io_lib:format("<pre>~60p</pre>", [T1:get(request)])),
%
%   Out:send(io_lib:format("<pre>~60p</pre>", [T1:get(response)])),
%
%   Out:send(""),
%
%   {ok, _State}.


