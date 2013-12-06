-module(ifl).

-compile(export_all).

-define(seq, {seq, fun ?MODULE:unary/1}).
-define(runs, 1).

payload() ->
  fib(27).

inputs() ->
  lists:seq(1, 100000).

fib(X) when X =< 1 ->
  X;
fib(X) ->
  fib(X-1) + fib(X-2).

decomp_for(Stages) ->
  fun(X) ->
    lists:duplicate(Stages, X)
  end.

recomp([X|_]) ->
  X.

unary(X) ->
  payload(),
  X.

binary(X, _Y) ->
  payload(),
  X.

test_pipe(Stages) ->
  lists:duplicate(Stages, ?seq).

test_farm(Stages) ->
  [{farm, [?seq], Stages}].

test_map(Stages) ->
  [{cluster, [{farm, [?seq], Stages}], decomp_for(Stages), fun ?MODULE:recomp/1}].

test_reduce(Stages) ->
  [{reduce, fun ?MODULE:binary/2, decomp_for(Stages)}].


benchmark() ->
  {ok, VerboseLog} = file:open("ifl.verbose.10.log", [write]),
  {ok, DataLog} = file:open("ifl.data.10.log", [write]),
  io:format("Starting tests!~n"),
  PayLoadResults = sk_profile:benchmark(fun ?MODULE:payload/0, [], 100),
  io:format(VerboseLog, "Payload: ~n\t~w~n", [PayLoadResults]),
  io:format(DataLog, "payload.dat: ~.5f~n", [proplists:get_value(mean, PayLoadResults)]),
  [benchmark(PipeLine, Stages, DataLog, VerboseLog) ||
    PipeLine <- [test_pipe, test_farm, test_map, test_reduce],
    Stages <- [1,2,4,8,16]],
  io:format("Done Tests!~n").

benchmark(PipeLine, Stages, DataLog, VerboseLog) ->
  Results = [bm_both(PipeLine, Stages, Schedulers) || Schedulers <- [1,2,4,8]],
  data_log(PipeLine, Stages, Results, standard_io),
  data_log(PipeLine, Stages, Results, DataLog),
  verbose_log(PipeLine, Stages, Results, VerboseLog).

bm_both(PipeLine, Stages, Schedulers) ->
  erlang:system_flag(schedulers_online, Schedulers),
  PipeLineX = ?MODULE:PipeLine(Stages),
  {Schedulers, bm_sequential(PipeLineX), bm_parallel(PipeLineX)}.

bm_parallel(PipeLine) ->
  Inputs = inputs(),
  sk_profile:benchmark(fun() ->
    sk_assembler:run(PipeLine, Inputs),
    receive
      {sink_results, Results} -> Results
    end
  end, [], ?runs).

bm_sequential(PipeLine) ->
  Inputs = inputs(),
  sk_profile:benchmark(fun() ->
    sk_assembler_sequential:run(PipeLine, Inputs),
    receive
      {sink_results, Results} -> Results
    end
  end, [], ?runs).

data_log(PipeLine, Stages, Results, DataLog) ->
  io:format(DataLog, "~w.~w.dat: ", [PipeLine, Stages]),
  [io:format(DataLog, "~B ~.5f ", [Schedulers, speedup(SeqResults, ParResults)]) ||
    {Schedulers, SeqResults, ParResults} <- Results],
  io:format(DataLog, "~n", []).

verbose_log(PipeLine, Stages, Results, VerboseLog) ->
  [io:format(VerboseLog,
             "PL: ~w; Stages: ~w; SEQUENTIAL; Schedulers: ~w~n\t~w~n" ++
             "PL: ~w; Stages: ~w; PARALLEL  ; Schedulers: ~w~n\t~w~n",
             [PipeLine, Stages, Schedulers, SeqResults] ++
             [PipeLine, Stages, Schedulers, ParResults]) ||
    {Schedulers, SeqResults, ParResults} <- Results].

speedup(SeqResults, ParResults) ->
  proplists:get_value(mean, SeqResults) / proplists:get_value(mean, ParResults).


