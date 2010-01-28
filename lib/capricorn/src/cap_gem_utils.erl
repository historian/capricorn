-module(cap_gem_utils).
-include("capricorn.hrl").


-export([match_requirements/2, find_matches/2, match_requirement/2]).


%%% External API
find_matches(Versions, Requirements) ->
  find_matches(Versions, Requirements, []).

find_matches([], _Requirements, Acc) ->
  Acc;

find_matches([Version|Other], Requirements, Acc) ->
  case match_requirements(Version, Requirements) of
  true  -> find_matches(Other, Requirements, [Version|Acc]);
  false -> find_matches(Other, Requirements, Acc)
  end.


match_requirements(_, []) ->
  true;

match_requirements(Version, [Req|Rest]) ->
  case match_requirement(Version, Req) of
  true  -> match_requirements(Version, Rest);
  false -> false
  end.


%%% Internal API
-spec balance_versions(version(), version()) -> {version(), version()}.
balance_versions({A},{B}) ->
  if
  length(A) > length(B) ->
    case lists:nth(length(B)+1, A) of
    E when is_list(E) ->
      balance_versions({A}, {B++[[255]]});
    _ ->
      balance_versions({A}, {B++[0]})
    end;
  length(A) < length(B) -> 
    case lists:nth(length(A)+1, B) of
    E when is_list(E) ->
      balance_versions({A++[[255]]}, {B});
    _ ->
      balance_versions({A++[0]}, {B})
    end;
  length(A) == length(B) ->
    {{A},{B}};
  true ->
    {{A},{B}}
  end.


-spec release_version(version()) -> version().
release_version({Parts}) ->
  {release_version(Parts, [])}.

-spec release_version(version_parts(), list()) -> version_parts().
release_version([], Acc) ->
  lists:reverse(Acc);
release_version([Part|_], Acc) when is_list(Part) ->
  lists:reverse(Acc);
release_version([Part|Rest], Acc) when is_integer(Part) ->
  release_version(Rest, [Part|Acc]).


-spec bump_version(version()) -> version().
bump_version(Version) ->
  {Parts1} = release_version(Version),
  Parts2 = if
    length(Parts1) > 1 ->
      lists:sublist(Parts1, length(Parts1)-1);
    true -> Parts1
  end,
  [Last|Rest] = lists:reverse(Parts2),
  Parts3 = lists:reverse([Last+1|Rest]),
  {Parts3}.

-spec match_requirement(version(), requirement()) -> boolean().
match_requirement(Version, {'=', RVersion}) ->
  {Version1, RVersion1} = balance_versions(Version, RVersion),
  Version1 == RVersion1;

match_requirement(Version, {'!=', RVersion}) ->
  {Version1, RVersion1} = balance_versions(Version, RVersion),
  Version1 /= RVersion1;

match_requirement(Version, {'<', RVersion}) ->
  {Version1, RVersion1} = balance_versions(Version, RVersion),
  Version1 < RVersion1;

match_requirement(Version, {'>', RVersion}) ->
  {Version1, RVersion1} = balance_versions(Version, RVersion),
  Version1 > RVersion1;

match_requirement(Version, {'>=', RVersion}) ->
  {Version1, RVersion1} = balance_versions(Version, RVersion),
  Version1 >= RVersion1;

match_requirement(Version, {'<=', RVersion}) ->
  {Version1, RVersion1} = balance_versions(Version, RVersion),
  Version1 =< RVersion1;

match_requirement(Version, {'~>', RVersion}) ->
  Version1  = release_version(Version),
  RVersion1 = bump_version(RVersion),
  match_requirement(Version1, {'>=', RVersion}) and 
  match_requirement(Version1, {'<',  RVersion1}).
