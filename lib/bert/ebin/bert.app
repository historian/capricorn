%% This is the application resource file (.app file) for the bert,
%% application.
{application, bert, 
  [{description, "BERT encoder / decoder"},
   {vsn, "0.1.0"},
   {modules, [bert]},
   {registered,[]},
   {applications, [kernel, stdlib]}
]}.