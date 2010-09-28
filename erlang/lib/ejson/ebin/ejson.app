%% This is the application resource file (.app file) for the bert,
%% application.
{application, ejson,
  [{description, "JSON encoder / decoder"},
   {vsn, "0.1.0"},
   {modules, [ejson, ejson_decode, ejson_encode]},
   {registered,[]},
   {applications, [kernel, stdlib]}
]}.