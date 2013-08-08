-module(sk_pipe_test).

-include_lib("eunit/include/eunit.hrl").
-include("sk_test_utils.hrl").
-compile(export_all).


simple_pipeline_example_test() ->
    ?assertEqual( [ 1,4,9,16,25 ],
                  skel:do([{seq , fun(X) -> X*X end}],
                          [1,2,3,4,5])).

pipeline_implemented_as_seq_undenetch_test() ->
    ?assertEqual( skel:do([{ seq, fun(X) -> X*X end}],
                          [1,2,3,4,5]),
                  skel:do([{ pipe ,[ fun(X) -> X*X end ]}],
                          [1,2,3,4,5])).

pipe_keeps_data_order_test() ->
    ?assertEqual( [ a,b,c,d,e,f ],
                  skel:do([ { pipe , [?random_sleep_for(100)]}],
                          [ a,b,c,d,e,f ])).

pipe_keeps_fun_calls_order_test() ->
    ?assertEqual([[ 'a',b,c,d,e ],
                  [ 'A',b,c,d,e ]],
                 skel:do([{pipe, [{ seq, fun(X) -> X ++ [b] end },
                                  { seq, fun(X) -> X ++ [c] end },
                                  { seq, fun(X) -> X ++ [d] end },
                                  { seq, fun(X) -> X ++ [e] end }]}],
                         [['a'], ['A'] ])).

