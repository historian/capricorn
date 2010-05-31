{application, emq, [
  {description, "Erlang Message Queue"},
  {vsn, "0.1.0"},
  {modules, [
    emq,
    emq_app,
    emq_sup,
    emq_pool,
    emq_queue,
    emq_status
  ]},
  {registered,[
    emq_sup
  ]},
  {applications, [kernel, stdlib]},
  {mod, {emq_app, []}}
]}.