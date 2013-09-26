-module(skel_test).

-include_lib("eunit/include/eunit.hrl").
-include("sk_test_utils.hrl").

macro_same_data_differen_order_test() ->
    ?assertSameDataInside( [ a,b,c,d ],
                           [ c,b,a,d ]).

