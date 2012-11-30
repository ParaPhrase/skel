%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains `map` skeleton partitioner logic.
%%%
%%% The `map` skeleton, takes each input, decomposes it, and puts each of the 
%%% decomposed parts through their own inner skeletons. After they have gone 
%%% through the inner skeletons, they are recomposed into a single input before
%%% being forwarded to the next skeleton.
%%%
%%% The partitioner splits each input according to a developer-defined decomp
%%% function, and then sends one part to each inner skeleton.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_map_partitioner).

-export([
         start/3
        ]).

-include("skel.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-spec start(skel:decomp_fun(), skel:workflow(), pid()) -> 'eos'.
start(Partitioner, WorkFlow, CombinerPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{combiner, CombinerPid}]),
  DataFun = sk_data:decomp_by(Partitioner),
  loop(DataFun, WorkFlow, CombinerPid, []).

-spec loop(skel:data_decomp_fun(), skel:workflow(), pid(), [pid()]) -> 'eos'.
loop(DataPartitionerFun, WorkFlow, CombinerPid, WorkerPids) ->
  receive
    {data, _, _} = DataMessage ->
      PartitionMessages = DataPartitionerFun(DataMessage),
      WorkerPids1 = start_workers(length(PartitionMessages), WorkFlow, CombinerPid, WorkerPids),
      Ref = make_ref(),
      sk_tracer:t(60, self(), {?MODULE, data}, [{ref, Ref}, {input, DataMessage}, {partitions, PartitionMessages}]),
      dispatch(Ref, length(PartitionMessages), PartitionMessages, WorkerPids1),
      loop(DataPartitionerFun, WorkFlow, CombinerPid, WorkerPids1);
    {system, eos} ->
      sk_utils:stop_workers(?MODULE, WorkerPids),
      eos
  end.

% Start more workers so we have enough workers for the partitions
-spec start_workers(pos_integer(), skel:workflow(), pid(), [pid()]) -> [pid()].
start_workers(NPartitions, WorkFlow, CombinerPid, WorkerPids) when NPartitions > length(WorkerPids) ->
  NNewWorkers = NPartitions - length(WorkerPids),
  NewWorkerPids = sk_utils:start_workers(NNewWorkers, WorkFlow, CombinerPid),
  NewWorkerPids ++ WorkerPids;
start_workers(_NPartitions, _WorkFlow, _CombinerPid, WorkerPids) ->
  WorkerPids.

% We can guarantee to always have enough workers by the time we get to this method.
-spec dispatch(reference(), pos_integer(), [skel:data_message(),...], [pid()]) -> 'ok'.
dispatch(Ref, NPartitions, PartitionMessages, WorkerPids) ->
  dispatch(Ref, NPartitions, 1, PartitionMessages, WorkerPids).

-spec dispatch(reference(), pos_integer(), pos_integer(), [skel:data_message(),...], [pid()]) -> 'ok'.
dispatch(_Ref,_NPartitions, _Idx, [], _) ->
  ok;
dispatch(Ref, NPartitions, Idx, [PartitionMessage|PartitionMessages], [WorkerPid|WorkerPids]) ->
  PartitionMessage1 = sk_data:push({decomp, Ref, Idx, NPartitions}, PartitionMessage),
  sk_tracer:t(50, self(), WorkerPid, {?MODULE, data}, [{partition, PartitionMessage1}]),
  WorkerPid ! PartitionMessage1,
  dispatch(Ref, NPartitions, Idx+1, PartitionMessages, WorkerPids).
