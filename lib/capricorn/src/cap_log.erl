%%
%% Application module
%%
%% File   : cap_log.erl
%% Created: 2010-01-04
%%
%% @author simonmenke <simon.menke@gmail.com>
%% @copyright 2010 simonmenke
%%
%% @doc TODO make nice description
%%

-module(cap_log).
-behaviour(gen_event).

-export([start_link/0,stop/0]).
-export([debug_on/0,info_on/0,get_level/0,get_level_integer/0, set_level/1]).
-export([init/1, handle_event/2, terminate/2, code_change/3, handle_info/2, handle_call/2]).

-define(LEVEL_ERROR, 3).
-define(LEVEL_INFO,  2).
-define(LEVEL_DEBUG, 1).
-define(LEVEL_TMI,   0).

level_integer(error) -> ?LEVEL_ERROR;
level_integer(info)  -> ?LEVEL_INFO;
level_integer(debug) -> ?LEVEL_DEBUG;
level_integer(tmi)   -> ?LEVEL_TMI;
level_integer(_Else) -> ?LEVEL_ERROR. % anything else default to ERROR level

level_atom(?LEVEL_ERROR) -> error;
level_atom(?LEVEL_INFO)  -> info;
level_atom(?LEVEL_DEBUG) -> debug;
level_atom(?LEVEL_TMI)   -> tmi.


start_link() ->
  cap_event_sup:start_link({local, cap_log}, error_logger, cap_log, []).

stop() ->
  cap_event_sup:stop(cap_log).

init([]) ->
  % read config and register for configuration changes
  
  % just stop if one of the config settings change. capricorn_server_sup
  % will restart us and then we will pick up the new settings.
  
  Filename = cap_config:get(log, file, "var/log/capricorn.log"),
  Level = cap_config:get(log, level, info),
  
  {ok, Fd} = file:open(Filename, [append]),
  {ok, {Fd, level_integer(Level)}}.

debug_on() ->
  get_level_integer() =< ?LEVEL_DEBUG.

info_on() ->
  get_level_integer() =< ?LEVEL_INFO.

set_level(LevelAtom) ->
  set_level_integer(level_integer(LevelAtom)).

get_level() ->
  level_atom(get_level_integer()).

get_level_integer() ->
  catch gen_event:call(error_logger, cap_log, get_level_integer).

set_level_integer(Int) ->
  gen_event:call(error_logger, cap_log, {set_level_integer, Int}).

handle_event({error_report, _, {Pid, capricorn_error, {Format, Args}}}, {Fd, _LogLevel}=State) ->
  log(Fd, Pid, error, Format, Args),
  {ok, State};
handle_event({error_report, _, {Pid, _, _}}=Event, {Fd, _LogLevel}=State) ->
  log(Fd, Pid, error, "~p", [Event]),
  {ok, State};
handle_event({error, _, {Pid, Format, Args}}, {Fd, _LogLevel}=State) ->
  log(Fd, Pid, error, Format, Args),
  {ok, State};
handle_event({info_report, _, {Pid, capricorn_info, {Format, Args}}}, {Fd, LogLevel}=State) when LogLevel =< ?LEVEL_INFO ->
  log(Fd, Pid, info, Format, Args),
  {ok, State};
handle_event({info_report, _, {Pid, capricorn_debug, {Format, Args}}}, {Fd, LogLevel}=State) when LogLevel =< ?LEVEL_DEBUG ->
  log(Fd, Pid, debug, Format, Args),
  {ok, State};
handle_event({_, _, {Pid, _, _}}=Event, {Fd, LogLevel}=State) when LogLevel =< ?LEVEL_TMI ->
  % log every remaining event if tmi!
  log(Fd, Pid, tmi, "~p", [Event]),
  {ok, State};
handle_event(_Event, State) ->
  {ok, State}.

handle_call(get_level_integer, {_Fd, LogLevel}=State) ->
  {ok, LogLevel, State};
handle_call({set_level_integer, NewLevel}, {Fd, _LogLevel}) ->
  {ok, ok, {Fd, NewLevel}}.

handle_info(_Info, State) ->
  {ok, State}.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

terminate(_Arg, {Fd, _LoggingLevel}) ->
  file:close(Fd).

log(Fd, Pid, Level, Format, Args) ->
  Msg = io_lib:format(Format, Args),
  ok = io:format("[~s] [~p] ~s~n", [Level, Pid, Msg]), % dump to console too
  Msg2 = re:replace(lists:flatten(Msg),"\\r\\n|\\r|\\n", "\n", [global, {return, list}]),
  ok = io:format(Fd, "[~s] [~s] [~p] ~s~n", [httpd_util:rfc1123_date(), Level, Pid, Msg2]).
