-module(sk_reduce_test).
-include_lib("eunit/include/eunit.hrl").

-include("sk_test_utils.hrl").

-compile(export_all).



example_reduce_run_test() ->
    ?assertSameDataInside([ 3,
                            7,
                            11],
                          skel:do([{reduce, 
                                    _ReduceFun = fun(X,Y) -> X+Y end ,
                                    _DecompositionFun = fun erlang:tuple_to_list/1}],
                                  [ { 1,2},
                                    {3,4},
                                    {5,6}])).

reduce_can_work_on_one_input_test() ->
    ?assertSameDataInside([ 3 ],
                          skel:do([{reduce, 
                                    fun(X,Y) -> X+Y end ,
                                    fun erlang:tuple_to_list/1}],
                                  [ { 1,2 } ])).

decomp_fun_needs_to_return_list_test() ->
    ?assertSameDataInside([ 3 ],
                          skel:do([{reduce, 
                                    fun(X,Y) -> X+Y end ,
                                    fun(X) -> X end  }],
                         [ [ 1,2 ] ])).
%% Second, failing case To hard to specify how error thrown by bad
%% workflow look like. This test is left out for future implementation
%% of linking and superaving work processes.
%% 
%% ?assertExit( function_clause,
%%                skel:do([{reduce, 
%%                          fun(X,Y) -> X+Y end ,
%%                          fun(X) -> X end  }],
%%                        [  1,2  ])).
    
    
reduce_can_handle_different_size_input_data_test() ->
    ?assertSameDataInside([ 3,
                            8,
                            100],
                          skel:do([{reduce, 
                                    fun(X,Y) -> X+Y end ,
                                    fun erlang:tuple_to_list/1}],
                                  [ { 1,2},
                                    { 1, 1, 1, 1, 1, 1, 1, 1},
                                    { 100 }])).

