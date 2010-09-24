#! /usr/bin/env escript

main([]) ->
    code:add_pathz("ebin/"),
    Cases = read_cases("test/timing.json"),
    io:format("Timing runs...~n", []),
    run(ejson, Cases),
    run(mochijson2, Cases),
    run(rfc4627, Cases).

run(Module, Cases) ->
    F = fun() ->
        lists:foreach(fun(_) ->
           lists:map(fun(C) -> Module:decode(C) end, Cases)
        end, lists:seq(1, 2000))
    end,
    io:format("~p => ~p~n", [Module, timed(F)]).

timed(Func) ->
    Start = micro(),
    Func(),
    End = micro(),
    (End-Start)/1000000.

micro() ->
    {Mega, Secs, Micro} = erlang:now(),
    (Mega * 1000000 + Secs) * 1000000 + Micro.

read_cases(FName) ->
    {ok, Data} = file:read_file(FName),
    read_cases(Data, [], []).

read_cases(Data, Curr, Acc) ->
    case Data of
        <<>> ->
            Case = list_to_binary(lists:reverse(Curr)),
            lists:reverse([Case | Acc]);
        <<"---", Rest/binary>> ->
            Case = list_to_binary(lists:reverse(Curr)),
            read_cases(Rest, [], [Case | Acc]);
        <<First:8/integer, Rest/binary>> ->
            read_cases(Rest, [First | Curr], Acc)
    end.