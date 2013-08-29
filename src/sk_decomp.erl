%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains the initialization logic of a `decomp` skeleton
%%%
%%% A `decomp` skeleton is just like a `map` skeleton only without any stream
%%% parallelism for the inner skeleton - ie each independent input is split 
%%% into multiple inputs for the inner skeleton, then when they emerge
%%% from the inner skeleton, all the inputs are grouped again corresponding to
%%% their original input.
%%%
%%% The function to split and the function to re-group are developer-provided.
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_decomp).

-export([
         make/3
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-spec make(skel:workflow(), skel:decomp_fun(), skel:recomp_fun()) -> fun((pid()) -> pid()).
make(WorkFlow, Decomp, Recomp) when is_function(Decomp, 1),
                                    is_function(Recomp, 1) ->
  fun(NextPid) ->
    RecompPid = spawn(sk_decomp_recomp, start, [Recomp, NextPid]),
    WorkerPid = sk_utils:start_worker(WorkFlow, RecompPid),
    spawn(sk_decomp_decomp, start, [Decomp, WorkerPid])
  end.
