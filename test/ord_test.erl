-module(ord_test).

-include_lib("eunit/include/eunit.hrl").
-compile( export_all).

failing_order_test() ->
    List = [a,b,c,d,f,g],
    ?assertNot( List =:=
                  skel:do( [{ farm, [ test_utils:random_sleep_for( 100 )],
                              2}],
                           List)).

simple_order_test() ->
    List = [a,b,c,d,f,g],
    ?assertEqual( List,
                  skel:do( [{ ord,
                              [{ farm, [ test_utils:random_sleep_for( 100 )],
                                 2}]}],
                           List)).
