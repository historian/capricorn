#! /usr/bin/env escript

main([]) ->
    code:add_pathz("ebin/"),
    check_good(),
    check_errors().

check_good() ->
    lists:map(fun
        ({J, E}) ->
             E = ejson:decode(J),
            J = ejson:encode(E);
        ({J, E, J2}) ->
            E = ejson:decode(J),
            J2 = ejson:encode(E)
        end,
    good()).

check_errors() ->
    lists:map(fun(E) ->
        ok = case (catch ejson:decode(E)) of
            {invalid_json, _} -> ok;
            Error ->
                io:format("Error: ~p~n", [E]),
                Error
        end
    end, errors()).

good() ->
    [
        {<<"[{}]">>, [{[]}]},
        {<<"{\"foo\":[123]}">>, {[{<<"foo">>, [123]}]}},
        {<<"{\"foo\":{\"bar\":true}}">>,
            {[{<<"foo">>, {[{<<"bar">>, true}]} }]} },
        {<<"{\"foo\":[],\"bar\":{\"baz\":true},\"alice\":\"bob\"}">>,
            {[
                {<<"foo">>, []},
                {<<"bar">>, {[{<<"baz">>, true}]}},
                {<<"alice">>, <<"bob">>}
            ]}
        },
        {<<"[-123,\"foo\",{\"bar\":[]},null]">>,
            [
                -123,
                <<"foo">>,
                {[{<<"bar">>, []}]},
                null
            ]
        }
    ].

errors() ->
    [
        <<"[{}">>,
        <<"}]">>
    ].
