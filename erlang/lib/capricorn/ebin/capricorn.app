{application, capricorn, [
  {description, "Capricorn application version 1.0"},
  {vsn, "0.1"},
  {modules, [
    capricorn_app,
    capricorn_sup,
    cap_vnode,
    cap_config,
    cap_gems,
    cap_web,
    cap_web_nodes
  ]},
  {registered, [
  ]},
  {applications, [kernel, stdlib, sasl, inets, ssl, riak_kv, luwak]},
  {mod, {capricorn_app, []}}
]}.