-module(libra_conf).
-include("libra.hrl").
-export([load/1]).

load(Path) ->
  load(Path, []).

load(Path, Watches) ->
  case file:consult(Path) of
  {ok, Options} ->
    handle_options(Options, Watches);
  Else -> Else
  end.


handle_options([], Watches) ->
  {ok, Watches};

handle_options([Option|Rest], Watches1) ->
  Watches2 = handle_option(Option, Watches1),
  handle_options(Rest, Watches2).


handle_option({load, Glob}, Watches1) ->
  lists:foldl(fun(Path, Watches2) ->
    load(Path, Watches2)
  end, Watches1, filelib:wildcard(Glob));

handle_option({watch, Name, Options}, Watches) ->
  Config1 = #config{
    name     = Name,
    uid      = "root",
    gid      = "root",
    wdir     = "/",
    stdout   = "/dev/null",
    stderr   = "/dev/null",
    pid_path = io_lib:format("/var/run/libra/~s.pid", [Name])
  },
  
  {StartCmd, StartGraceTime} =
  case proplists:lookup(start, Options) of
  {start, Cmd1, Grace1} ->
    {Cmd1, Grace1};
  {start, Cmd2} ->
    {Cmd2, 3};
  Otherwise1 ->
    error_logger:error_msg("Invalid start directive: ~p", [Otherwise1]),
    {undefined, 3}
  end,
  
  {StopCmd, StopGraceTime} =
  case proplists:lookup(stop, Options) of
  {stop, Cmd3, Grace3} ->
    {Cmd3, Grace3};
  {stop, Cmd4} ->
    {Cmd4, 3};
  none ->
    {"kill $PID", 3};
  Otherwise2 ->
    error_logger:error_msg("Invalid stop directive: ~p", [Otherwise2]),
    {undefined, 3}
  end,
  
  {RestartCmd, RestartGraceTime} =
  case proplists:lookup(restart, Options) of
  {restart, Cmd5, Grace5} ->
    {Cmd5, Grace5};
  {restart, Cmd6} ->
    {Cmd6, 3};
  none ->
    {undefined, 3};
  Otherwise3 ->
    error_logger:error_msg("Invalid restart directive: ~p", [Otherwise3]),
    {undefined, 3}
  end,
  
  Config2 = Config1#config{
    uid                = proplists:get_value(uid,    Options, Config1#config.uid),
    gid                = proplists:get_value(gid,    Options, Config1#config.gid),
    wdir               = proplists:get_value(wcd,    Options, Config1#config.wdir),
    pid_path           = proplists:get_value(pid,    Options, Config1#config.pid_path),
    stdout             = proplists:get_value(stdout, Options, Config1#config.stdout),
    stderr             = proplists:get_value(stderr, Options, Config1#config.stderr),
    start_cmd          = StartCmd,
    stop_cmd           = StopCmd,
    restart_cmd        = RestartCmd,
    start_grace_time   = StartGraceTime,
    stop_grace_time    = StopGraceTime,
    restart_grace_time = RestartGraceTime,
    daemonize          = proplists:get_bool(daemonize, Options)
  },
  
  [Config2|Watches].
