-module(fib).

-compile(export_all).

p(Input) ->
  _ = fib(22) + fib(20),
  Input.

f(Input) ->
  _ = fib(24),
  Input.

f_prime(Input) ->
  _ = fib(22),
  Input.

decomp(Input) ->
  lists:duplicate(4, Input).

recomp([Input|_]) ->
  Input.

% Computational Payload.
% All that matters is that it has predictable runtime and 100% CPU utilisation
fib(X) when X =< 1 ->
  X;
fib(X) ->
  fib(X-1) + fib(X-2) + fib(X-3) + fib(X-4).

run_all_examples() ->
  [run_examples(X) || X <- [8,6,4,2,1]].

run_examples(X) ->
  erlang:system_flag(schedulers_online, X),
  Ntimes = 10,
  Input = lists:duplicate(1024, xx),
  io:format("------~nRunning Examples on ~p cores. Please be Patient. ~n", [X]),
  io:format("Example 4: ~p~n", [sk_profile:benchmark(fun ?MODULE:example4/1, [Input], Ntimes)]),
  io:format("Example 3: ~p~n", [sk_profile:benchmark(fun ?MODULE:example3/1, [Input], Ntimes)]),
  io:format("Example 2: ~p~n", [sk_profile:benchmark(fun ?MODULE:example2/1, [Input], Ntimes)]),
  io:format("Example 1: ~p~n", [sk_profile:benchmark(fun ?MODULE:example1/1, [Input], Ntimes)]),
  io:format("Done with examples on ~p cores.~n------~n", [X]).

example1(Images) ->
  [f(p(Im)) || Im <- Images].

example2(Images) ->
  skel:do([{seq, fun ?MODULE:p/1}, {seq, fun ?MODULE:f/1}], Images).

example3(Images) ->
  skel:do([{seq, fun ?MODULE:p/1}, {map, [{seq, fun ?MODULE:f_prime/1}], fun ?MODULE:decomp/1, fun ?MODULE:recomp/1}], Images).

example4(Images) ->
  skel:do([{farm, [{seq, fun ?MODULE:p/1}], 4}, {map, [{seq, fun ?MODULE:f_prime/1}], fun ?MODULE:decomp/1, fun ?MODULE:recomp/1}], Images).
