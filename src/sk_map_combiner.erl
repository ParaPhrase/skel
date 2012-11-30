%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains `map` skeleton combiner logic.
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
-module(sk_map_combiner).

-export([
         start/2
        ]).

-include("skel.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-spec start(skel:recomp_fun(), pid()) -> 'eos'.
start(Combiner, NextPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{next_pid, NextPid}]),
  DataFun = sk_data:recomp_with(Combiner),
  loop(0, 0, dict:new(), DataFun, NextPid).

-spec loop(non_neg_integer(), non_neg_integer(), dict(), skel:data_recomp_fun(), pid()) -> 'eos'.
loop(TotWorkers, DeadWorkers, Dict, DataCombinerFun, NextPid) ->
  receive
    {data, _, _} = PartitionMessage ->
      {{decomp, Ref, Idx, NPartitions}, PartitionMessage1} = sk_data:pop(PartitionMessage),
      Dict1 = store(Ref, Idx, NPartitions, PartitionMessage1, Dict),
      Dict2 = combine_and_forward(Ref, Dict1, DataCombinerFun, NextPid),
      TotWorkers1 = new_total_workers(TotWorkers, NPartitions),
      loop(TotWorkers1, DeadWorkers, Dict2, DataCombinerFun, NextPid);
    {system, eos} when DeadWorkers+1 >= TotWorkers ->
      sk_tracer:t(75, self(), NextPid, {?MODULE, system}, [{msg, eos}, {total, TotWorkers}, {dead, DeadWorkers+1}]),
      NextPid ! {system, eos},
      eos;
    {system, eos} ->
      sk_tracer:t(85, self(), {?MODULE, system}, [{msg, eos}, {total, TotWorkers}, {dead, DeadWorkers+1}]),
      loop(TotWorkers, DeadWorkers+1, Dict, DataCombinerFun, NextPid)
  end.

-spec new_total_workers(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
new_total_workers(TotWorkers, NPartitions) when NPartitions > TotWorkers ->
  NPartitions;
new_total_workers(TotWorkers, _NPartitions) ->
  TotWorkers.

-spec store(reference(), pos_integer(), pos_integer(), skel:data_message(), dict()) -> dict().
store(Ref, Idx, NPartitions, PartitionMessage, Dict) ->
  Dict1 = dict:store({Ref, expecting}, NPartitions, Dict),
  Dict2 = dict:store({Ref, Idx}, PartitionMessage, Dict1),
  dict:update_counter({Ref, received}, 1, Dict2).

-spec combine_and_forward(reference(), dict(), skel:data_recomp_fun(), pid()) -> dict().
combine_and_forward(Ref, Dict, DataCombinerFun, NextPid) ->
  case dict:find({Ref, expecting}, Dict) of
    error             -> Dict;
    {ok, NPartitions} -> combine_and_forward(Ref, Dict, NPartitions, DataCombinerFun, NextPid)
  end.

-spec combine_and_forward(reference(), dict(), pos_integer(), skel:data_recomp_fun(), pid()) -> dict().
combine_and_forward(Ref, Dict, NPartitions, DataCombinerFun, NextPid) ->
  RcvdPartitions = dict:fetch({Ref, received}, Dict),
  if
    RcvdPartitions == NPartitions ->
      PartitionMessages = fetch_partitions(Ref, NPartitions, Dict, []),
      DataMessage = apply(DataCombinerFun, [PartitionMessages]),
      sk_tracer:t(50, self(), NextPid, {?MODULE, data}, [{ref, Ref}, {output, DataMessage}, {partitions, PartitionMessages}]),
      NextPid ! DataMessage,
      purge_partitions(Ref, NPartitions, Dict);
    true ->
      Dict
  end.

-spec fetch_partitions(reference(), non_neg_integer(), dict(), [any()]) -> [any()].
fetch_partitions(_Ref, 0, _Dict, Acc) ->
  Acc;
fetch_partitions(Ref, NPartitions, Dict, Acc) ->
  {ok, Piece} = dict:find({Ref, NPartitions}, Dict),
  fetch_partitions(Ref, NPartitions-1, Dict, [Piece|Acc]).

-spec purge_partitions(reference(), non_neg_integer(), dict()) -> dict().
purge_partitions(Ref, 0, Dict) ->
  Dict1 = dict:erase({Ref, expecting}, Dict),
  Dict2 = dict:erase({Ref, received}, Dict1),
  Dict2;
purge_partitions(Ref, NPartitions, Dict) ->
  Dict1 = dict:erase({Ref, NPartitions}, Dict),
  purge_partitions(Ref, NPartitions-1, Dict1).
