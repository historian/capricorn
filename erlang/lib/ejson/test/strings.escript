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
        {<<"\"\"">>, <<"">>},
        {<<"\"0\"">>, <<"0">>},
        {<<"\"foo\"">>, <<"foo">>},
        {<<"\"\\\"foobar\\\"\"">>, <<"\"foobar\"">>},
        {<<"\"\\n\\n\\n\"">>, <<"\n\n\n">>},
        {<<"\"\\\" \\b\\f\\r\\n\\t\\\"\"">>, <<"\" \b\f\r\n\t\"">>},
        {<<"\"foo\\u0005bar\"">>, <<"foo", 5, "bar">>},
        {<<"\"\\uFFFF\"">>, <<239, 191, 191>>},
        {<<"\"\\uD834\\uDD1E\"">>, <<240, 157, 132, 158>>},
        {<<"\"\\uD834foo\\uDD1E\"">>, <<237, 160, 180, "foo", 237, 180, 158>>}
    ].

errors() ->
    [
        <<"\"", 0, "\"">>,
        <<"\"\\g\"">>,
        % CouchDB-345
        <<"\"",78,69,73,77,69,78,32,70,216,82,82,32,70,65,69,78,33,"\"">>
    ].
