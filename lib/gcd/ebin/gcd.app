{application, gcd, [
  {description, "Grand Central Dispatch"},
  {vsn, "0.1"},
  {modules, [
    gcd_app,
    gcd_sup,
    gcd_srv,
    gcd_event,
    gcd_server,
    gcd
  ]},
  {registered, [
    gcd_srv,
    gcd_sup
  ]},
  {applications, [kernel, stdlib]},
  {mod, {gcd_app, []}}
]}.
