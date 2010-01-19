-module(libra_sys).
-include("libra.hrl").
-export([ps_aux/0]).


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