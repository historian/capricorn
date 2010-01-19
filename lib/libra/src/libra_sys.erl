-module(libra_sys).
-include("libra.hrl").

-export([start/1, stop/1]).
-export([ps_aux/0, alive/1]).
-export([read_pid_file/1]).

% ruby boot.rb User Group Wdir 0 Cmd
% ruby boot.rb User Group Wdir 1 PidPath Stdout Stderr Cmd
start(#config{daemonize=true}=Config) ->
  case clean_pid_file(Config) of
  {error, Reason} -> erlang:throw(Reason);
  ok -> ok
  end,
  
  Ruby = os:find_executable("ruby"),
  Boot = filename:join([code:priv_dir(libra), "boot.rb"]),
  R = os:cmd(string:join([Ruby, Boot,
    Config#config.uid,
    Config#config.gid,
    Config#config.wdir,
    "1",
    Config#config.pid_path,
    Config#config.stdout,
    Config#config.stderr,
    Config#config.start_cmd
  ], " ")),
  case R of
  "" -> wait_for_pid(Config);
  Error -> {error, {boot, Error}}
  end;

start(#config{daemonize=false}=Config) ->
  case clean_pid_file(Config) of
  {error, Reason} -> erlang:throw(Reason);
  ok -> ok
  end,
  
  Ruby = os:find_executable("ruby"),
  Boot = filename:join([code:priv_dir(libra), "boot.rb"]),
  R = os:cmd(string:join([Ruby, Boot,
    Config#config.uid,
    Config#config.gid,
    Config#config.wdir,
    "0",
    Config#config.start_cmd
  ], " ")),
  case R of
  "" -> wait_for_pid(Config);
  Error -> {error, {boot, Error}}
  end.

stop(_Config) ->
  ok.

ps_aux() ->
  Data = os:cmd("ps axo pid=,ppid=,pcpu=,rss="),
  Lines = string:tokens(Data, "\n"),
  ps_aux_parse_lines(Lines, []).

ps_aux_parse_lines([], Acc) -> Acc;
ps_aux_parse_lines([Line|Rest], Acc) ->
  [PidStr, PPidStr, CpuStr, RssStr] = string:tokens(Line, " "),
  
  {Pid, []}  = string:to_integer(PidStr),
  {PPid, []} = string:to_integer(PPidStr),
  {Cpu, []}  = string:to_float(CpuStr),
  {Rss, []}  = string:to_integer(RssStr),
  
  Stats = #stats{pid=Pid, ppid=PPid, cpu=Cpu, rss=Rss},
  
  ps_aux_parse_lines(Rest, [Stats|Acc]).


clean_pid_file(#config{pid_path=PidPath}) ->
  clean_pid_file(PidPath);
clean_pid_file(PidPath) ->
  case alive(PidPath) of
  true  -> {error, process_alive};
  false ->
    file:delete(PidPath),
    ok
  end.


alive(#config{pid_path=PidPath}) ->
  alive(PidPath);
alive(PidPath) when is_list(PidPath) ->
  case read_pid_file(PidPath) of
  {ok, Pid} -> alive(Pid);
  Else      -> Else
  end;
alive(Pid) when is_integer(Pid) ->
  case os:cmd(io_lib:format("kill -0 ~p", [Pid])) of
  []    -> true;
  _Else -> false
  end.


read_pid_file(#config{pid_path=PidPath}) ->
  read_pid_file(PidPath);
read_pid_file(PidPath) ->
  case catch file:read_file(PidPath) of
  {ok, PidData} ->
    case string:to_integer(PidData) of
    {Pid, _} -> {ok, Pid};
    _Else    ->
      catch file:delete(PidPath),
      {error, no_pid}
    end;
  _Else -> {error, no_pid}
  end.


wait_for_pid(Config) ->
  wait_for_pid(Config, 0).
wait_for_pid(Config, Seconds) ->
  if (Seconds > Config#config.start_grace_time) ->
    {timout};
  true ->
    case file:read_file(Config#config.pid_path) of
    {ok, PidData} ->
      case string:to_integer(PidData) of
      {Pid,_} -> {ok, Pid};
      _Else   -> 
        erlang:sleep(1000),
        wait_for_pid(Config, Seconds+1)
      end;
    _Else ->
      erlang:sleep(1000),
      wait_for_pid(Config, Seconds+1)
    end
  end.
