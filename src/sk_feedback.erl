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
         make/2
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-spec make(skel:workflow(), skel:filter_fun()) -> skel:maker_fun().
make(WorkFlow, FilterFun) ->
  fun(NextPid) ->
    Ref = make_ref(),
    CounterPid = spawn(sk_feedback_bicounter, start, []),
    FilterPid = spawn(sk_feedback_filter, start, [FilterFun, Ref, CounterPid, NextPid]),
    WorkerPid = sk_utils:start_worker(WorkFlow, FilterPid),
    spawn(sk_feedback_receiver, start, [Ref, CounterPid, FilterPid, WorkerPid])
  end.
