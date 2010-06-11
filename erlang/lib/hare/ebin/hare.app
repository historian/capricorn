{application, hare, [
  {description, "HTTP/WebSockets Server"},
  {vsn, "0.1.0"},
  {modules, [
    hare,
    hare_server,
    hare_utils
  ]},
  {registered,[]},
  {applications, [kernel, stdlib, sasl]},
  {included_applications, [crypto]}
]}.