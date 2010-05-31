%% This is the application resource file (.app file) for the bertio,
%% application.
{application, bertio, 
  [{description, "BERT Port"},
   {vsn, "0.1.0"},
   {modules, [bertio]},
   {registered,[]},
   {applications, [kernel, stdlib, bert]}
]}.