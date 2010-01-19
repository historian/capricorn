
-record(state,    {local,remote,nodes,monitors}).
-record(service,  {pid,node,location,name,description}).
-record(node,     {name}).
-record(monitor,  {ref,pid,pattern}).
