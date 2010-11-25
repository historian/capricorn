-define(b2a(V), list_to_atom(binary_to_list(V))).
-define(a2b(V), list_to_binary(atom_to_list(V))).
-define(b2l(V), binary_to_list(V)).
-define(l2b(V), list_to_binary(V)).

-define(LOG_DEBUG(Format, Args),
  case cap_log:debug_on() of
  true -> error_logger:info_report(capricorn_debug, {Format, Args});
  false -> ok
  end).

-define(LOG_INFO(Format, Args),
  case cap_log:info_on() of
  true -> error_logger:info_report(capricorn_info, {Format, Args});
  false -> ok
  end).

-define(LOG_ERROR(Format, Args),
  error_logger:error_report(capricorn_error, {Format, Args})).
