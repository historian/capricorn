{application, hatter, [
  {description, "Rack Container"},
  {vsn, "0.1.0"},
  {modules, [
    hatter,
    hatter_app,
    hatter_sup,
    
    hatter_afs_volume,
    hatter_afs_tar_volume,
    hatter_container,
    hatter_driver,
    hatter_driver_conn,
    hatter_driver_rack,
    hatter_console,
    
    hatter_config_srv,
    hatter_services_srv,
    hatter_http_srv,
    hatter_afs_srv,
    hatter_container_srv,
    hatter_container_sup,
    hatter_locator_srv,
    hatter_cache_srv,
    
    esi_lang_scanner,
    esi_lang_parser,
    esi_lang_processor,
    esi_headers_parser,
    esi_headers_scanner,
    esi_headers
  ]},
  {registered,[
    hatter_sup
  ]},
  {applications, [kernel, stdlib, sasl]},
  {included_applications, [crypto, mochiweb]},
  {mod, {hatter_app, []}}
]}.