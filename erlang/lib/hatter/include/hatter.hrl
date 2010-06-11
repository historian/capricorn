
-type container() :: {node(), atom()} .
-type service()   :: {node(), atom(), atom()} .

-record(hatter_req, {
  container_id     :: container(),
  container_conf   :: dict(),
  mochi_req        :: term(),
  mochi_resp       :: term(),
  req_handlers=[]  :: [{function(), term()}],
  resp_handlers=[] :: [{function(), term()}]
}).
-type hatter_req() :: #hatter_req{} .

-type hatter_resp() :: {
  integer(),
  dict(),
  {chunked, function()} | {integer(), function()} | binary(),
  hatter_req()} .

