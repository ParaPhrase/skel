%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains `ord` skeleton initialization logic.
%%%
%%% The `ord` skeleton can reorder outputs from its inner skeletons such that
%%% they have the same order coming out the ord skeleton as they had going into
%%% it.
%%%
%%% This becomes useful when requiring ordering on things like a farm.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_ord).

-export([
         make/2
        ]).


-ifdef(TEST).
-compile(export_all).
-endif.



-spec make(skel:worker_fun(), list() ) -> skel:maker_fun().
make( WorkerFun, [] ) ->
  make( WorkerFun ).


-spec make(skel:workflow()) -> skel:maker_fun().
make(WorkFlow) ->
  fun(NextPid) ->
    ReordererPid = spawn(sk_ord_reorderer, start, [NextPid]),
    WorkerPid = sk_utils:start_worker(WorkFlow, ReordererPid),
    spawn(sk_ord_tagger, start, [WorkerPid])
  end.
