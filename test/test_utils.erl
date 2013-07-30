-module(test_utils).

-include_lib("eunit/include/eunit.hrl").

-compile(export_all).


random_sleep_for( MaxTime ) ->
    fun( X ) ->
            timer:sleep( random_int_up_to( MaxTime ) ),
            X
    end.


random_int_up_to ( Max ) ->
    erlang:round( random:uniform() * Max ).





