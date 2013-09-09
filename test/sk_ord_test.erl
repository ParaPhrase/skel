-module(sk_ord_test).

-include_lib("eunit/include/eunit.hrl").

-include("sk_test_utils.hrl").

-compile( export_all).


failing_order_test() ->
  List = [a,b,c,d,f,g],
  ?assertNot( List =:=
                skel:do( [{ farm,
                            [{do, [ ?random_sleep_for( 100 )]},
                             {workers, 2}]}],
                         List)).

simple_order_test() ->
  List = [a,b,c,d,f,g],
  ?assertEqual( List,
                skel:do( [{ ord,
                            [{ do, [{farm,
                                     [{do, [ ?random_sleep_for( 100 )]},
                                      {workers, 2}]}]} ] }],
                         List)).
