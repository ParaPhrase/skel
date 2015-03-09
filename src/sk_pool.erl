%%%----------------------------------------------------------------------------
%%% @author Ramsay Taylor <r.g.taylor@sheffield.ac.uk>
%%%
%%% @doc This module contains the pool skeleton - a work farmer for unbalanced clusters
%%%
%%% This skeleton requests a number of worker processes from the sk_work_master. It then
%%% allocates one job to each worker and waits for it to complete. As each worker completes
%%% its task it is assigned another. This allows more tasks to be assigned to faster workers.
%%%
%%% This assumes that the tasks themselves take a long time relative to reporting their completion
%%% and assigning new tasks. For short tasks it may be more efficient to use sk_farm, or to combine
%%% tasks into blocks.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_pool).

-export([make/2]).

-include("skel.hrl").

-spec make((pos_integer()|{exactly,pos_integer()}|{max,pos_integer()}), workflow()) -> maker_fun().
make(NWorkers, WorkFlow) ->
  fun(NextPid) ->
	  WorkerPids = sk_work_master:reserve(NWorkers,WorkFlow,collector_to_follow),
	  EmitterPid = spawn(sk_pool_emitter, start, [WorkerPids]),
	  CollectorPid = spawn(sk_pool_collector, start, [NWorkers, EmitterPid, NextPid]),
	  lists:map(fun(W) -> W ! {set_collector,CollectorPid} end, WorkerPids),
	  EmitterPid
  end.


