
-define( assertSameDataInside( Expected, Given),
         lists:map( fun(Element) -> ?is_in(Element, Given) end,
                    Expected)).

-define ( is_in( Element, List), 
          ?assert(lists:any( fun(Any) -> Any =:= Element end,
                             List))).
    
