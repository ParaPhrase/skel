-module(sk_seq_test).

-include_lib("eunit/include/eunit.hrl").
-include("sk_test_utils.hrl").
-compile(export_all).


simple_sequance_example_test() ->
    ?assertEqual( [ 1,4,9,16,25 ],
                  skel:do([{seq , fun(X) -> X*X end}],
                          [1,2,3,4,5])).

seq_semanitcs_for_new_interface() ->
    ?assertEqual( [ 1,4,9,16,25 ],
                  skel:do([{seq , [ {do, fun(X) -> X*X end } ]}],
                          [1,2,3,4,5])).

seq_tuple_can_be_ommited_with_same_effect_test() ->
    ?assertEqual( skel:do([ fun(X) -> X*X end],
                          [1,2,3,4,5]),
                  skel:do([{seq , fun(X) -> X*X end}],
                          [1,2,3,4,5])).

seq_keeps_data_order_test() ->
    ?assertEqual( [ a,b,c,d,e,f ],
                  skel:do([{seq ,?random_sleep_for(100)}],
                          [ a,b,c,d,e,f ])).

seq_keeps_fun_calls_order_test() ->
    ?assertEqual([[ 'a',b,c,d,e ],
                  [ 'A',b,c,d,e ]],
                 skel:do([{ seq, fun(X) -> X ++ [b] end },
                          { seq, fun(X) -> X ++ [c] end },
                          { seq, fun(X) -> X ++ [d] end },
                          { seq, fun(X) -> X ++ [e] end }],
                          [['a'], ['A'] ])).
