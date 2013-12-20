-module(sk_cluster_test).


-include_lib("eunit/include/eunit.hrl").
-include("sk_test_utils.hrl").
-compile(export_all).


simple_example_of_decomp_usage_test()->
  ?assertSameDataInside( [ {0,3,6},
                           {9,12,15} ],
                         skel:do( [{ cluster,
                                     [ fun (X) -> X*3 end],
                                     fun erlang:tuple_to_list/1,
                                     fun erlang:list_to_tuple/1}] ,
                                  [ {0,1,2},
                                    {3,4,5} ] )).

