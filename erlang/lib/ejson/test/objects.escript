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
        {<<"{}">>, {[]}},
        {<<"{\"foo\": \"bar\"}">>,
            {[{<<"foo">>, <<"bar">>}]},
            <<"{\"foo\":\"bar\"}">>},
        {<<"\n\n{\"foo\":\r \"bar\",\n \"baz\"\t: 123 }">>,
            {[{<<"foo">>, <<"bar">>}, {<<"baz">>, 123}]},
            <<"{\"foo\":\"bar\",\"baz\":123}">>}
    ].

errors() ->
    [
        <<"{">>,
        <<"{,}">>,
        <<"{123:true}">>,
        <<"{false:123}">>,
        <<"{:\"stuff\"}">>,
        <<"{\"key\":}">>,
        <<"{\"key\": 123">>,
        <<"{\"key\": 123 true">>,
        <<"{\"key\": 123,}">>
    ].
