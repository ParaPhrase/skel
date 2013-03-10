-module(sumEuler).

-compile(export_all).

gcd(A, 0) -> A;
gcd(A, B) -> gcd(B, A rem B).


relPrime(X,Y) -> gcd(X, Y) == 1.

mkList(N) -> lists:seq(1,N).


euler(N) -> length( lists:filter(fun(X) -> ?MODULE:relPrime(N,X) end, mkList(N))).


sumEuler(N) -> lists:sum(lists:map(fun ?MODULE:euler/1,mkList(N))).

mapEuler(List) -> lists:map(fun ?MODULE:sumEuler/1, List).

parSumEuler(N) -> skel:do([{farm, [{seq, fun ?MODULE:euler/1}], 10}], mkList(N)).

run_examples(X, Y) ->
  erlang:system_flag(schedulers_online, X),
  io:format("Example 4: ~p~n", [sk_profile:benchmark(fun ?MODULE:parSumEuler/1, [Y], 1)]),
  io:format("Done with examples on ~p cores.~n------~n", [X]).


chunk(List, X, X, ChunkSize) -> [];
chunk(List, Start, Len, ChunkSize) ->
 lists:append([lists:sublist(List, Start, ChunkSize)], [chunk(List, Start+ChunkSize, Len, ChunkSize)]).
