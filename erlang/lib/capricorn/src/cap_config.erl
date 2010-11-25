-module(cap_config).

-export([
  set/3,
  get/2, get/3,
  unset/2
]).


set(Env, Var, Value) ->
  Key = normalize_key(Env, Var),
  {ok, C} = riak:local_client(),
  
  case C:get(<<"cap.config">>, Key) of
  {ok, OldObject} ->
    NewObject = OldObject:update_value(Value),
    C:put(NewObject, 3);
  _Else ->
    NewObject = riak_object:new(<<"cap.config">>, Key, Value),
    C:put(NewObject, 3)
  end.


get(Env, Var) ->
  get(Env, Var, undefined).

get(Env, Var, Default) ->
  {ok, C} = riak:local_client(),
  case do_get(Env, Var, C) of
  {ok, Value} -> Value;
  _Else -> Default
  end.


unset(Env, Var) ->
  {ok, C} = riak:local_client(),
  C:delete(<<"cap.config">>, normalize_key(Env, Var)).
  


do_get(Env, Var, C) ->
  Key = normalize_key(Env, Var),
  case C:get(<<"cap.config">>, Key) of
  {ok, Object} ->
    Value = riak_object:get_value(Object),
    {ok, Value};
  _Else ->
    case fallback_env(Env, C) of
    {ok, Fallback} ->
      do_get(Fallback, Var, C);
    _Else2 ->
      {error, not_found}
    end
  end.


fallback_env(Env, C) ->
  Key = normalize_key(Env, "_fallback"),
  case C:get(<<"cap.config">>, Key) of
  {ok, Object} ->
    Fallback = riak_object:get_value(Object),
    {ok, Fallback};
  _Else ->
    {error, no_fallback}
  end.


normalize_key(Env, Var) ->
  Env1 = normalize_env(Env),
  Var1 = normalize_var(Var),
  <<Env1/binary, ".", Var1/binary>>.

normalize_env(Env) when is_binary(Env) -> normalize_env(binary_to_list(Env));
normalize_env(Env) when is_atom(Env)   -> normalize_env(atom_to_list(Env));
normalize_env(Env) when is_list(Env)   -> list_to_binary(string:to_upper(Env)).

normalize_var(Var) when is_binary(Var) -> normalize_var(binary_to_list(Var));
normalize_var(Var) when is_atom(Var)   -> normalize_var(atom_to_list(Var));
normalize_var(Var) when is_list(Var)   -> list_to_binary(string:to_upper(Var)).
