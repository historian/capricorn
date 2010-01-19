%% This is the application resource file (.app file) for the bertio,
%% application.
{application, libra, 
  [{description, "System Monitoring"},
   {vsn, "0.1.0"},
   {modules, [
    libra_sys,
    libra_monitor,
    libra_watcher
   ]},
   {registered,[]},
   {applications, [kernel, stdlib]}
]}.