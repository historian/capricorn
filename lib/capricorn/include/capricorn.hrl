%%
%% Include file
%%
%% File   : capricorn.hrl
%% Created: 2010-01-04
%%
%% @author simonmenke <simon.menke@gmail.com>
%% @copyright 2010 simonmenke
%%
%% @doc TODO make nice description
%%

-define(b2a(V), list_to_atom(binary_to_list(V))).
-define(b2l(V), binary_to_list(V)).
-define(l2b(V), list_to_binary(V)).

-define(LOG_DEBUG(Format, Args),
  case capricorn_log:debug_on() of
  true -> error_logger:info_report(capricorn_debug, {Format, Args});
  false -> ok
  end).

-define(LOG_INFO(Format, Args),
  case capricorn_log:info_on() of
  true -> error_logger:info_report(capricorn_info, {Format, Args});
  false -> ok
  end).

-define(LOG_ERROR(Format, Args),
  error_logger:error_report(capricorn_error, {Format, Args})).

-record(cluster, {
  node=undefined
}).

-record(machine, {
  name=undefined,
  node=undefined
}).

-record(version, {
  parts=[0] :: [pos_integer()|string(),...]
}).
-type version() :: #version{}.

-record(gem_ref, {
  name=undefined    :: 'undefined' | binary(),
  version=undefined :: 'undefined' | version(),
  lib=undefined     :: 'undefined' | binary()
}).
-type gem_ref() :: #gem_ref{}.

-record(application, {
  id=undefined            :: 'undefined' | binary(),
  node=undefined          :: 'undefined' | atom(),
  name=undefined          :: 'undefined' | binary(),
  domains=[]              :: [binary(),...],
  environment=development :: atom(),
  www_user=undefined      :: 'undefined' | binary(),
  www_group=undefined     :: 'undefined' | binary(),
  root_path=undefined     :: 'undefined' | binary(),
  installed_gems=[]       :: [gem_ref()],
  required_gems=[]        :: [binary()],
  rvsn={rvsn, 0}
}).
-type application() :: #application{}.

-record(gem_id, {
  name=undefined,
  version=#version{}
}).

-record(gem, {
  id=#gem_id{},
  deps=[],
  missing=[],
  rvsn={rvsn, 0}
}).

-record(dependency, {
  name=undefined,
  reqs=[]
}).

-record(requirement, {
  op=(<<">=">>),
  version=#version{}
}).
