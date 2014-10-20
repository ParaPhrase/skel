
-define( assertSameDataInside( Expected, Given),
         lists:map( fun(Element) -> ?is_in(Element, Given) end,
                    Expected)).

-define ( is_in( Element, List), 
          ?assert(lists:any( fun(Any) -> Any =:= Element end,
                             List))).
    



-define(random_sleep_for( MaxTime ),
        fun( X ) ->
                timer:sleep( ?random_int_up_to( MaxTime ) ),
                X
        end).


-define(random_int_up_to ( Max ) ,
        erlang:round( random:uniform() * Max )).



