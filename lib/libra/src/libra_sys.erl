-module(libra_sys).
-include("libra.hrl").
-export([ps_aux/0]).

% ruby boot.rb User Group Wdir 0 Cmd
% ruby boot.rb User Group Wdir 1 PidPath Stdout Stderr Cmd
start(Config#config{daemonize=true}) ->
  Ruby = os:find_executable("ruby"),
  Boot = filename:join([code:priv_dir(libra), "boot.rb"]),
  R = os:cmd([Ruby, Boot,
    Config#config.uid
    Config#config.gid
    Config#config.wdir,
    "1",
    Config#config.pid_path,
    Config#config.stdout,
    Config#config.stderr,
    Config#config.start_cmd
  ]),
  case R of
  "" -> wait_for_pid(Config);
  Error -> {error, {boot, Error}}
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
        wait_for_pid(Config, Seconds+1);
    _Else ->
      erlang:sleep(1000),
      wait_for_pid(Config, Seconds+1)
    end
  end.

stop() ->
  

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