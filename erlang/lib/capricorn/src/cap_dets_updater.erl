-module(cap_dets_updater).

-export([update/2]).

update(Table, Fun) ->
  NewTerms = dets:foldl(fun(Term, Acc) ->
    update_members([Term], Fun, Acc)
  end, [], Table),
  dets:delete_all_objects(Table),
  dets:insert(Table, NewTerms),
  ok.

update_members([], _Fun, Acc) -> Acc;
update_members([OldTerm|Rest], Fun, Acc) ->
  case Fun(OldTerm) of
  ok ->
    update_members(Rest, Fun, [OldTerm|Acc]);
  remove ->
    update_members(Rest, Fun, Acc);
  {update, Terms} when is_list(Terms) ->
    update_members(Terms++Rest, Fun, Acc);
  {update, Term} when is_tuple(Term) ->
    update_members([Term|Rest], Fun, Acc);
  Else ->
    erlang:error({badarg, Else})
  end.
