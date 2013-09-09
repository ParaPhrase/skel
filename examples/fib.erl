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
  [run_examples(X) || X <- [4,2,1]].


run_examples(X) ->
  erlang:system_flag(schedulers_online, X),
  Ntimes = 5,
  Input = lists:duplicate(24, xx),
  io:format("------~nRunning Examples on ~p cores. Please be Patient. ~n", [X]),
  io:format("Example 4: ~p~n",
            [ benchmark( example1, Input, Ntimes) ]),
  io:format("Example 3: ~p~n",
            [ benchmark( example2, Input, Ntimes) ]),
  io:format("Example 2: ~p~n",
            [ benchmark( example3, Input, Ntimes) ]),
  io:format("Example 1: ~p~n",
            [ benchmark( example4, Input, Ntimes) ]),
  io:format("Done with examples on ~p cores.~n------~n", [X]).

benchmark( Name, Input, Ntimes ) ->
  sk_profile:benchmark(fun ?MODULE:Name/1, [Input], Ntimes).

example1(Images) ->
  [f(p(Im)) || Im <- Images].

example2(Images) ->
  skel:do([{seq, fun ?MODULE:p/1},
           {seq, fun ?MODULE:f/1}],
          Images).

example3(Images) ->
  skel:do([{seq, fun ?MODULE:p/1},
           {map,
            [{do, [{seq, fun ?MODULE:f_prime/1}]},
             {decomp, fun ?MODULE:decomp/1},
             {recomp, fun ?MODULE:recomp/1}] }],
          Images).

example4(Images) ->
  skel:do([{farm,
            [{do, [{seq, fun ?MODULE:p/1}] }, 
             {workers, 4}]},
           {map,
            [{do, [{seq, fun ?MODULE:f_prime/1}]},
             {decomp, fun ?MODULE:decomp/1},
             {recomp, fun ?MODULE:recomp/1}] }],
          Images).
