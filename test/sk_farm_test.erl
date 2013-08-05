-module(sk_farm_test).


-include_lib("eunit/include/eunit.hrl").
-include("sk_test_utils.hrl").
-compile(export_all).


simple_example_of_farm_usage_test()->
    ?assertSameDataInside( [ 0,3,6,9,12  ],
                           skel:do( [{farm,
                                      [ fun (X) -> X*3 end],
                                      4}] ,
                                    [ 0,1,2,3,4 ]  )).
