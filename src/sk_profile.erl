%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% 
%%% @doc This module contains various functions to help us when profiling and
%%% benchmarking.
%%%
%%% This allows us very simple access to benchmarking.
%%% 
%%% === Example ===
%%% 
%%%   ```sk_profile:benchmark(skel:run([{seq, fun ?MODULE:p/1}, {seq, fun ?MODULE:f/1}], Images), [Input], Ntimes)])'''
%%%
%%%   In this example we use the {@link benchmark/3} function to record how 
%%%   long it takes for the example seen in {@link sk_seq} to execute.
%%%
%%% @end
%%% @todo Include eprof functionality
%%%----------------------------------------------------------------------------
-module(sk_profile).

-export([
         benchmark/3
        ]).

-include("skel.hrl").

-spec benchmark(fun(), list(), pos_integer()) -> list().
%% @doc Produces a list of averages for the time taken by function `Fun' to be 
%% evaluated `N' times, given a list of arguments `Args'. Returned times are 
%% in microseconds. Returns a list containing the tuples:
%% 
%% <ul>
%% <li><tt>N</tt>,</li>
%% <li><tt>min</tt>, the shortest time taken to perform Fun;</li>
%% <li><tt>max</tt>, the longest time taken to perform Fun;</li>
%% <li><tt>med</tt>, the median of all individual results;</li> 
%% <li><tt>mean</tt>, the mean of all individual results; and</li>
%% <li><tt>std_dev</tt>, the standard deviation.</li>
%% </ul>
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

%% @doc Recursively records the length of time it takes for the function `Fun'
%% to be evaluated `N' times.
test_loop(_Fun, _Args, 0, Timings) ->
  Timings;
test_loop(Fun, Args, N, Timings) ->
  {Timing, _} = timer:tc(Fun, Args),
  test_loop(Fun, Args, N-1, [Timing|Timings]).

-spec median([number(),...]) -> number().
%% @doc Returns the median of the times listed.
median(List) ->
  lists:nth(round((length(List) / 2)), lists:sort(List)).

-spec mean([number(),...]) -> number().
%% @doc Returns the mean time taken for those listed.
mean(List) ->
  lists:foldl(fun(X, Sum) -> X + Sum end, 0, List) / length(List).

-spec std_dev([number(),...], number()) -> number().
%% @doc Returns the standard deviation of all times recorded.
std_dev(List, Mean) ->
  math:pow(variance(List, Mean), 0.5).

-spec variance([number(),...], number()) -> number().
%% @doc Calculates the variance of the times listed for use in calculating the 
%% standard deviation in {@link std_dev/2}.
variance(List, _Mean) when length(List) == 1 ->
  0.0;
variance(List, Mean) ->
  lists:foldl(fun(X, Sum) -> math:pow(Mean - X, 2) + Sum end, 0, List) / (length(List) - 1).
