-module(cap_config).
-include("capricorn.hrl").



-export([all/0, get/1, get/2, get/3]).



all() ->
  application:get_all_env(capricorn) ++ init:get_arguments().



get(Section) ->
  proplists:get_value(Section, cap_config:all(), []).

get(Section, Key) ->
  cap_config:get(Section, Key, undefined).

get(Section, Key, Default) ->
  SectionValues = cap_config:get(Section),
  proplists:get_value(Key, SectionValues, Default).


