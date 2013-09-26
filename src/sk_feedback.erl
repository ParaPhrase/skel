%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains `feedback` skeleton initialization logic.
%%%
%%% The `feedback` skeleton contains an inner skeleton. Inputs are repeatedly
%%% sent through the inner skeleton until the developer-defined feedback
%%% function returns false for the input.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_feedback).

-export([
         make/1
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-export_type([ workflow/0,
               filter_fun/0]).

-type workflow() :: { feedback, [ propertiy(), ...]}.

-type propertiy() :: { do, skel:workflow() } |
                     { while, filter_fun() }.

-type filter_fun()  :: fun((any())        -> boolean()).



-spec make( [propertiy(), ...] ) -> skel:maker_fun().
make(Proplist) ->
  make ( _WorkFlow = proplists:get_value( do, Proplist),
         _Filter = proplists:get_value( while, Proplist)).


-spec make(skel:workflow(), filter_fun()) -> skel:maker_fun().
make(WorkFlow, Filter) when is_function(Filter, 1) ->
  fun(NextPid) ->
    Ref = make_ref(),
    CounterPid = spawn(sk_feedback_bicounter, start, []),
    FilterPid = spawn(sk_feedback_filter, start, [Filter, Ref, CounterPid, NextPid]),
    WorkerPid = sk_utils:start_worker(WorkFlow, FilterPid),
    spawn(sk_feedback_receiver, start, [Ref, CounterPid, FilterPid, WorkerPid])
  end.
