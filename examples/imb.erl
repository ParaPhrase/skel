-module(imb).
-compile(export_all).

-include("imb.hrl").

run(Benchmark) ->
    run(Benchmark, 0).

run(Benchmark, Processes) ->
    BenchmarkFun = benchmark_decode(Benchmark, Processes),
    Repetitions = repetition_generate(),
    Data = data_generate(),
    FormatBenchmark = format_benchmark_decode(Benchmark),
    FormatHeading = format_heading_decode(Benchmark),
    Headings = heading_decode(Benchmark),
    print_heading(FormatHeading, Headings),
    [exec_benchmark(BenchmarkFun, R, D, FormatBenchmark) || {R, D} <- lists:zip(Repetitions, Data)].

heading_decode(sendrecv) ->
    ["#bytes", "#repetitions", "t_min[usec]", "t_max[usec]", "t_avg[usec]", "Mbytes/sec"];
heading_decode(alltoall) ->
    ["#bytes", "#repetitions", "t_min[usec]", "t_max[usec]", "t_avg[usec]", "Mbytes/sec"];
heading_decode(_) ->
    ["#bytes", "#repetitions", "t[usec]", "Mbytes/sec"].

format_heading_decode(sendrecv) ->
    "~15s ~15s ~15s ~15s ~15s ~15s~n";
format_heading_decode(alltoall) ->
    "~15s ~15s ~15s ~15s ~15s ~15s~n";
format_heading_decode(_) ->
    "~15s ~15s ~15s ~15s~n".

format_benchmark_decode(sendrecv) ->
    "~15w ~15w ~15.2f ~15.2f ~15.2f ~15.2f~n";
format_benchmark_decode(alltoall) ->
    "~15w ~15w ~15.2f ~15.2f ~15.2f ~15.2f~n";
format_benchmark_decode(_) ->
    "~15w ~15w ~15.2f ~15.2f~n".

benchmark_decode(pingpong, _) ->
    fun(R, D) -> pingpong:run(R, D) end;
benchmark_decode(pingping, _) ->
    fun(R, D) -> pingping:run(R, D) end;
benchmark_decode(sendrecv, P) when P > 0 ->
    fun(R, D) -> sendrecv:run(R, D, P) end;
benchmark_decode(alltoall, P) when P > 0 ->
    fun(R, D) -> alltoall:run(R, D, P) end.

exec_benchmark(BenchmarkFun, R, D, Format) ->
    Time = BenchmarkFun(R, D),
    if
        is_list(Time) ->
            TimeMin = lists:min(Time),
            TimeMax = lists:max(Time),
            TimeAvg = lists:sum(Time) / length(Time), 
            MbytesPerSec = bandwidth_calc(D, TimeAvg),
            io:format(Format, [size(D), R, TimeMin, TimeMax, TimeAvg, MbytesPerSec]);
        is_float(Time) ->
            MbytesPerSec = bandwidth_calc(D, Time),
            io:format(Format, [size(D), R, Time, MbytesPerSec])
    end.

bandwidth_calc(Data, Time) ->
    Megabytes = (size(Data) / math:pow(2, 20)),
    Seconds = (Time * 1.0e-6),
    Megabytes / Seconds.

print_heading(Format, Headings) ->
    io:format(Format, Headings).

repetition_generate() ->
    Thousands = [1000 || _ <- lists:seq(1, 17)],
    lists:append(Thousands, [640, 320, 160, 80, 40, 20, 10]).

data_generate() ->
    [bytes_generate(0) | [bytes_generate(round(math:pow(2, X))) || X <- lists:seq(0, ?ITERATIONS - 2)]].

bytes_generate(Size) ->
    bytes_generate(Size, []).

bytes_generate(0, Bytes) ->
    list_to_binary(Bytes);
bytes_generate(Size, Bytes) ->
    bytes_generate(Size - 1, [1 | Bytes]).

time_microseconds() ->
    {MS, S, US} = now(),
    (MS * 1.0e+12) + (S * 1.0e+6) + US.

finalize(P) ->
    receive
        {done, P} -> 
            ?TRACE("DONE: pid: ~p from: ~p ~n", [self(), P]),
            ok
    end.
