-module(reduce).

-compile(export_all).

% Computational Payload.
% All that matters is that it has predictable runtime and 100% CPU utilisation
fib(X) when X =< 1 ->
  X;
fib(X) ->
  fib(X-1) + fib(X-2) + fib(X-3) + fib(X-4).

id(X) ->
  X.

reduce(X, Y) ->
  fib(21),
  X + Y.

benchmark() ->
  io:format("Starting tests!~n", []),
  io:format("ReduceFn: ~w~n", [sk_profile:benchmark(fun ?MODULE:reduce/2, [0,0], 100)]),
  NT = 5,
  [bm(NS, FN, NI, PPI, NT) || NS <- [16,8], NI <- [1, 100], PPI <- [32, 64, 128], FN <- [parallel, sequential]].

bm(NSchedulers, Fn, NInputs, PartsPerInput, NTimes) ->
  erlang:system_flag(schedulers_online, NSchedulers),
  Inputs = lists:duplicate(NInputs, lists:seq(1, PartsPerInput)),
  Results = sk_profile:benchmark(fun ?MODULE:Fn/1, [Inputs], NTimes),
  io:format("~p: (~p Schedulers; ~p Inputs; ~p Parts): ~n\t~w~n", [Fn, NSchedulers, NInputs, PartsPerInput, Results]),
  Results.


parallel(Inputs) ->
  skel:do([{reduce, fun ?MODULE:reduce/2, fun ?MODULE:id/1}], Inputs).


sequential(Inputs) ->
  [fold1(fun ?MODULE:reduce/2, Input) || Input <- Inputs ].


% Implemented as a treefold underneath

-spec fold1(fun((A, A) -> A), [A,...]) -> A when A :: term().
%% @doc Sequential `reduce' entry-point. Primarily for comparison purposes.
fold1(_ReduceFun, [L1]) ->
  L1;
fold1(ReduceFun, [L1, L2 | List]) ->
  fold1(ReduceFun, [ReduceFun(L1, L2) | pairs(ReduceFun, List)]).

-spec pairs(fun((A, A) -> A), [A]) -> [A] when A :: term().
%% @doc Second stage to {@link fold1}'s  sequential `reduce'. Recursively
%% pairs the first two elements in the list and applies the given function
%% `Fun'.
pairs(Fun, [L1,L2|List]) ->
  [Fun(L1,L2) | pairs(Fun, List)];
pairs(_Fun, List) ->
  List.
