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
    lists:map(fun({Json, Reason}) ->
        ok = case (catch ejson:decode(Json)) of
            {invalid_json, {invalid_number, Reason}} -> ok;
            {invalid_json, Reason} -> ok;
            Error ->
                Fmt = "Wrong error for ~p: ~p (exp: {invalid_number, ~p})~n",
                io:format(standard_error, Fmt, [Json, Error, Reason]),
                Error
        end
    end, errors()).

good() ->
    [
        {<<"0">>, 0},
        {<<"1">>, 1},
        {<<"12">>, 12},
        {<<"-3">>, -3},
        {<<"309230948234098">>, 309230948234098},
        {<<"1.0">>, 1.0},
        {<<"0.3">>, 0.3},
        {<<"2.4234324">>, 2.4234324},
        {<<"-3.1416">>, -3.1416},
        {<<"1E4">>, 10000.0, <<"1.0e4">>},
        {<<"1.0E+01">>, 10.0, <<"10.0">>},
        {<<"1e1">>, 10.0, <<"10.0">>},
        {<<"3.0E2">>, 300.0, <<"300.0">>},
        {<<"0E3">>, 0.0, <<"0.0">>},
        {<<"1.5E3">>, 1500.0, <<"1.5e3">>},
        {<<"1.5E-1">>, 0.15, <<"0.15">>},
        {<<"-0.323E+2">>, -32.3, <<"-32.3">>}
    ].

errors() ->
    [
        {<<"02">>, leading_zero},
        {<<"-0">>, no_more_data},
        {<<"-01">>, "-01"},
        {<<"+12">>, plus_prefix_is_invalid},
        {<<"0A">>, leading_zero},
        {<<".1">>, decimals_require_a_zero},
        {<<"0.A">>, not_digit_after_decimal},
        {<<"1A">>, invalid_digit},
        {<<"1.A">>, not_digit_after_decimal},
        {<<"-">>, lonely_hyphen},
        {<<"1.">>, missing_fraction},
        {<<"1.-1">>, not_digit_after_decimal},
        {<<"1E">>, invalid_exponent},
        {<<"1-E2">>, invalid_digit},
        {<<"2E +3">>, no_exponent_after_e},
        {<<"1EA">>, invalid_digit_in_exponent}
    ].
