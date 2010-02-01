%% This is the application resource file (.app file) for the bertrpc,
%% application.
{application, bertrpc, 
  [{description, "BERT-RPC Server"},
   {vsn, "0.1.0"},
   {modules, [bertrpc,
              bertrpc_module,
              bertrpc_server,
              bertrpc_connection]},
   {registered,[]},
   {applications, [kernel, stdlib, bert]}
]}.

