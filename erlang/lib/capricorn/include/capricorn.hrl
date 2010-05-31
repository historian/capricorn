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


-type version_part()  :: pos_integer() | string() .
-type version_parts() :: [version_part(),...] .
-type version()       :: {version_parts()} .
-type requirement()   :: {'='|'!='|'<'|'>'|'>='|'<='|'~>', version()} .
-type dependency()    :: {binary(), [requirement()]} .


-record(gem_ref, {
  name    :: binary(),
  version :: 'undefined' | version(),
  lib     :: 'undefined' | binary()
}).
-type gem_ref() :: #gem_ref{}.

-record(gem_id, {
  name    :: binary(),
  version :: version()
}).
-type gem_id() :: #gem_id{}.

-record(application, {
  id=undefined            :: 'undefined' | binary(),
  node=undefined          :: 'undefined' | atom(),
  name=undefined          :: 'undefined' | binary(),
  domains=[]              :: [binary(),...],
  environment=development :: atom(),
  www_user=undefined      :: 'undefined' | binary(),
  www_group=undefined     :: 'undefined' | binary(),
  root_path=undefined     :: 'undefined' | binary(),
  installed_gems=[]       :: [gem_id()],
  required_gems=[]        :: [binary()],
  rvsn={rvsn, 0}
}).
-type application() :: #application{}.


-record(gem, {
  id             :: gem_id(),
  deps=[]        :: [dependency()],
  missing=[]     :: [dependency()],
  rvsn={rvsn, 1}
}).
-type gem_spec() :: #gem{} .

