%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains the initialization logic of a `farm` skeleton.
%%%
%%% A farm skeleton has the most basic kind of stream parallelism - inputs are
%%% sent to one of `n` replicas of the inner skeleton for processing.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_farm).

-export([
         make/2
        ]).

-include("skel.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-spec make(pos_integer(), skel:workflow()) -> skel:maker_fun().
make(NWorkers, WorkFlow) ->
  fun(NextPid) ->
    CollectorPid = spawn(sk_farm_collector, start, [NWorkers, NextPid]),
    WorkerPids = sk_utils:start_workers(NWorkers, WorkFlow, CollectorPid),
    spawn(sk_farm_emitter, start, [WorkerPids])
  end.
