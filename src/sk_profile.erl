%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains various functions to help us when profiling and
%%% benchmarking.
%%%
%%% This allows us very simple access to benchmarking.
%%%
%%% @end
%%% @todo Include eprof functionality
%%%----------------------------------------------------------------------------
-module(sk_profile).

-export([
         benchmark/3
        ]).


-ifdef(TEST).
-compile(export_all).
-endif.

-spec benchmark(fun(), list(), pos_integer()) -> list().
benchmark(Fun, Args, N) when N > 0 ->
  Timing = test_loop(Fun, Args, N, []),
  Mean = mean(Timing),
  [
    {n, N},
    {min, lists:min(Timing)},
    {max, lists:max(Timing)},
    {med, median(Timing)},
    {mean, Mean},
    {std_dev, std_dev(Timing, Mean)}
  ].

test_loop(_Fun, _Args, 0, Timings) ->
  Timings;
test_loop(Fun, Args, N, Timings) ->
  {Timing, _} = timer:tc(Fun, Args),
  test_loop(Fun, Args, N-1, [Timing|Timings]).

-spec median([number(),...]) -> number().
median(List) ->
  lists:nth(round((length(List) / 2)), lists:sort(List)).

-spec mean([number(),...]) -> number().
mean(List) ->
  lists:foldl(fun(X, Sum) -> X + Sum end, 0, List) / length(List).

-spec std_dev([number(),...], number()) -> number().
std_dev(List, Mean) ->
  math:pow(variance(List, Mean), 0.5).

-spec variance([number(),...], number()) -> number().
variance(List, _Mean) when length(List) == 1 ->
  0.0;
variance(List, Mean) ->
  lists:foldl(fun(X, Sum) -> math:pow(Mean - X, 2) + Sum end, 0, List) / (length(List) - 1).
