-module(hatter_vclock, [Entries]).


lookup(Id) ->
  proplists:get_value(Id, Entries, 0).

store(Id, Rev) ->
  L = proplists:delete(Id, Entries),
  hatter_vclock:new([{Id, Rev} | L]).

delete(Id) ->
  L = proplists:delete(Id, Rev),
  hatter_vclock:new(L).

increment(Id) ->
  Rev = lookup(Id),
  store(Id, Rev + 1).

keys() ->
  get_keys(Entries).

cmp(Other) ->
  AllKeys = lists:usort(keys() ++ Other:keys()).
  lists:foldl(fun
  (Key,  0) ->
    L = lookup(Key),
    R = Other:lookup(Key),
    if
    (L > R) -> -1;
    (L < R) ->  1;
    (L = R) ->  0
    end;
  (Key,  1) ->
    L = lookup(Key),
    R = Other:lookup(Key),
    if
    (L > R) ->  conflict;
    (L < R) ->  1;
    (L = R) ->  1
    end;
  (Key, -1) ->
    L = lookup(Key),
    R = Other:lookup(Key),
    if
    (L > R) -> -1;
    (L < R) ->  conflict;
    (L = R) -> -1
    end
  end, 0, AllKeys).
