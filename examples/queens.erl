-module(queens).

-compile(export_all).


run(N) -> rainhas(N).

rainhas(N) -> lists:map(fun(X) -> search(N, X) end, lists:seq(1, N)).

search(Numero, N) -> lists:takewhile( fun (A) -> head(A) == N end, prainhas(Numero, N)).

head([H|T]) -> H.

prainhas(Numero, Linha) 
    -> rainhas2(Numero, Linha, Numero).
    
check({C,L}, {I,J}) -> (L == J) or (C+L == I+J) or (C-L == I-J).

safe(P,N) -> 
   M = (length(P)) + 1,
   List = [ not(check({I,J}, {M,N})) || {I,J} <- lists:zip(lists:seq(1, length(P)), P) ],
   lists:foldr(fun(X,Y) -> X and Y end, true, List). 
    
rainhas2(0, Linha, Numero) -> [[]];
rainhas2(M, Linha, Numero) -> [lists:append(P, [N]) || P <- rainhas2(M-1, Linha, Numero), N <- lists:append(lists:seq(Linha, Numero), lists:seq(1, Linha-1)), safe(P, N)].

parRun(N) -> skel:run([{farm, [{seq, fun(X) -> ?MODULE:search(N, X) end}], 12}], lists:seq(1,N)),
             receive
                    {sink_results, Results} -> Results
             end.

run_examples(X) ->
  erlang:system_flag(schedulers_online, X),
  io:format("Example 4: ~p~n", [sk_profile:benchmark(fun ?MODULE:parRun/1, [12], 1)]),
  io:format("Done with examples on ~p cores.~n------~n", [X]).




