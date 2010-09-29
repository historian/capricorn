{application, ep2p, [
  {description, "Erlang Peer 2 peer Library"},
  {vsn, "0.1"},
  {modules, [
    ep2p,
    inet_ep2p_dist,
    ep2p_contact_list
  ]},
  {registered, [
    ep2p_contact_list
  ]},
  {applications, [
    kernel,
    stdlib,
    ssl
  ]}
]}.
