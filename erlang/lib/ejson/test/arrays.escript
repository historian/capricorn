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
        {<<"[]">>, []},
        {<<"[\t[\n]\r]">>, [[]], <<"[[]]">>},
        {<<"[\t123, \r true\n]">>, [123, true], <<"[123,true]">>},
        {<<"[1,\"foo\"]">>, [1, <<"foo">>]},
        {<<"[1199344435545.0,1]">>, [1199344435545.0,1]},
        {<<"[\"\\u00A1\",\"\\u00FC\"]">>, [<<194, 161>>, <<195, 188>>]}
    ].

errors() ->
    [
        <<"[">>,
        <<"]">>,
        <<"[,]">>,
        <<"[123">>,
        <<"[123,]">>,
        <<"[32 true]">>
    ].
