%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains what happens in the recomposer process of a 
%%% `decomp` skeleton. 
%%%
%%% A `decomp` skeleton is just like a `map` skeleton only without any stream
%%% parallelism for the inner skeleton.
%%%
%%% The recomposer process combines a multiple input into a single input using
%%% a developer-provided function.
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_decomp_recomp).

-export([
         start/2
        ]).

-include("skel.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-spec start(skel:recomp_fun(), pid()) -> 'eos'.
start(Recomp, NextPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{next_pid, NextPid}]),
  DataRecompFun = sk_data:recomp_with(Recomp),
  loop(dict:new(), DataRecompFun, NextPid).

-spec loop(dict(), skel:data_recomp_fun(), pid()) -> 'eos'.
loop(Dict, DataRecompFun, NextPid) ->
  receive
    {data, _, _} = PartitionMessage ->
      {{decomp, Ref, Idx, NPartitions}, PartitionMessage1} = sk_data:pop(PartitionMessage),
      Dict1 = store(Ref, Idx, NPartitions, PartitionMessage1, Dict),
      Dict2 = combine_and_forward(Ref, Dict1, DataRecompFun, NextPid),
      loop(Dict2, DataRecompFun, NextPid);
    {system, eos} ->
      sk_tracer:t(75, self(), NextPid, {?MODULE, system}, [{msg, eos}]),
      NextPid ! {system, eos},
      eos
  end.

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
