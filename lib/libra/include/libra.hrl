
-record(stats, { record_version=0,
  pid,
  ppid,
  cpu,
  rss
}).

-record(process, { record_version=0,
  pid,
  ppid,
  history=[],
  children=[]
}).

-record(config, { record_version=0,
  name,
  
  stdout,
  stderr,
  
  uid,
  gid,
  
  daemonize=true,
  wdir,
  pid_path,
  
  start_cmd,
  stop_cmd="kill $PID",
  restart_cmd,
  
  start_grace_time=3,
  stop_grace_time=3,
  restart_grace_time=3
}).
