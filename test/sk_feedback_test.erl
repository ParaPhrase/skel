-module(sk_feedback_test).


-include_lib("eunit/include/eunit.hrl").
-include("sk_test_utils.hrl").
-compile(export_all).

simple_example_of_feedback_usage_test()->
    ?assertSameDataInside( [ 5,5,5,5,5,6,7,8,9,10 ],
                           skel:do( [{ feedback,
                                       [ fun (X) -> X+1 end],
                                       fun (X) -> X<5 end }] ,
                                    [ 0,1,2,3,4,5,6,7,8,9,10 ] )).


