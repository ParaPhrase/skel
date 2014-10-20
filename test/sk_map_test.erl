-module(sk_map_test).


-include_lib("eunit/include/eunit.hrl").
-include("sk_test_utils.hrl").
-compile(export_all).


simple_example_of_map_pattern_usage_test()  ->
  ?assertSameDataInside( [ [0, 2],
                           [4,6],
                           [20, 40] ],
                         skel:do( [{ map, [ fun double/1 ]}],
                                  [[0,1],
                                   [2,3],
                                   [10, 20] ])).

double( X ) -> X*2.


map_can_handle_different_size_input_data_test() ->
  Given = [[0],
           [2,3],
           [10, 20, 30 ]],
  Expected = [[0],
              [4,6],
              [20, 40, 60 ] ],
  ?assertSameDataInside( Expected,
                         skel:do( [{ map, [ fun double/1 ] }],
                                 Given )).


map_can_be_given_number_of_workers_as_parameter_test()  ->
  Given = [[0],
           [2,3],
           [10, 20, 30 ]],
  Expected = [[0],
              [4,6],
              [20, 40, 60 ] ],

  ?assertSameDataInside( Expected,
                         skel:do( [{ map, [ fun double/1 ], _NumberOfWorkers = 8}],
                                  Given )).
