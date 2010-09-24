#! /usr/bin/env escript

main([]) ->
    code:add_pathz("ebin/"),
    ok = true(),
    ok = false(),
    ok = null().

true() ->
    true = ejson:decode(<<"true">>),
    case ejson:encode(true) of
        <<"true">> -> ok;
        Error -> throw({invalid_result, Error})
    end.

false() ->
    false = ejson:decode(<<"false">>),
    case ejson:encode(false) of
        <<"false">> -> ok;
        Error -> throw({invalid_result, Error})
    end.

null() ->
    null = ejson:decode(<<"null">>),
    case ejson:encode(null) of
        <<"null">> -> ok;
        Error -> throw({invalid_result, Error})
    end.    


