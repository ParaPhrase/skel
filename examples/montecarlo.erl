-module(montecarlo).
%-compile(export_all).
-export([run/4]).

-ifdef(debug).
-define(TRACE(Format, Data), io:format(string:concat("TRACE ~p:~p ", Format), [?MODULE, ?LINE] ++ Data)).
-else.
-define(TRACE(Format, Data), void).
-endif.

run(SamplesTotal, ProcTotal, BoundLower, BoundUpper) ->
    TimeStart = time_seconds(),
    Processes = [
        spawn(fun() -> montecarlo(SamplesTotal, ProcNum, ProcTotal, BoundLower, BoundUpper) end) 
            || 
        ProcNum <- lists:seq(0, ProcTotal - 1)
    ],
    [Process ! {calc, self()} || Process <- Processes],
    ResultSum = lists:sum(finalize(Processes)),
    Result = montecarlo_finalize(SamplesTotal, BoundLower, BoundUpper, ResultSum),
    TimeEnd = time_seconds(),
    TimeElapsed = TimeEnd - TimeStart,
    io:format("result: ~.6f time: ~.6f~n", [Result, TimeElapsed]).

montecarlo(SamplesTotal, ProcNum, ProcTotal, BoundLower, BoundUpper) ->
    receive 
        {calc, Parent} ->
            random_seed(ProcNum),
            Result = samples_process(SamplesTotal, ProcNum, ProcTotal, BoundLower, BoundUpper),
            ?TRACE(
                "CALC(~p): SamplesTotal: ~p ProcNum: ~p ProcTotal: ~p BoundLower: ~p BoundUpper: ~p Result: ~p~n", 
                [self(), SamplesTotal, ProcNum, ProcTotal, BoundLower, BoundUpper, Result]
            ),
            Parent ! {done, Result, self()}
    end.

finalize(Processes) ->
    finalize(Processes, []).

finalize(Processes, Results) ->
    receive
        {done, Result, From} ->
            IsValid = lists:member(From, Processes),
            if 
                IsValid and ((length(Results) + 1) =:= length(Processes)) ->
                    ?TRACE("DONE (final): from: ~p result: ~p results: ~p~n", [From, Result, Results]),
                    [Result | Results];
                IsValid ->
                    ?TRACE("DONE: from: ~p result: ~p results: ~p~n", [From, Result, Results]),
                    finalize(Processes, [Result | Results]);
                true ->
                    finalize(Processes, Results)
            end
    end.

samples_calc(SamplesTotal, ProcNum, ProcTotal) when ProcNum < ProcTotal ->
    SamplesBase = SamplesTotal div ProcTotal,
    SamplesRemainder = SamplesTotal rem ProcTotal,
    if
        ProcNum < SamplesRemainder ->
            SamplesBase + 1;
        true ->
            SamplesBase
    end.

samples_process(SamplesTotal, ProcNum, ProcTotal, BoundLower, BoundUpper) ->
    Samples = samples_calc(SamplesTotal, ProcNum, ProcTotal),
    lists:sum([fun_calc(random_value(BoundLower, BoundUpper)) || _ <- lists:seq(1, Samples)]).

montecarlo_finalize(SamplesTotal, BoundLower, BoundUpper, Results) ->
    Results * ((BoundUpper - BoundLower) / SamplesTotal).

fun_calc(X) ->
    (1.0 / (math:sqrt(2.0 * math:pi()))) * math:exp(-1.0 * (math:pow(X, 2.0) / 2.0)).

random_value(BoundLower, BoundUpper) when BoundLower < BoundUpper ->
    (random:uniform() * (BoundUpper - BoundLower)) + BoundLower.

random_seed(X) ->
    {S1, S2, S3} = now(),
    random:seed(S1 + X, S2 + X, S3 + X).

time_seconds() ->
    {MS, S, US} = now(),
    (MS * 1.0e+6) + S + (US * 1.0e-6).
