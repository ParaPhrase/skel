%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains what happens in the decomposer process of a 
%%% `decomp` skeleton. 
%%%
%%% A `decomp` skeleton is just like a `map` skeleton only without any stream
%%% parallelism for the inner skeleton.
%%%
%%% The decomposer process splits a single input into multiple inputs using
%%% a developer-provided function.
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_decomp_decomp).

-export([
         start/2
        ]).

-include("skel.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-spec start(skel:decomp_fun(), pid()) -> 'eos'.
start(Decomp, NextPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{next_pid, NextPid}]),
  DataDecompFun = sk_data:decomp_by(Decomp),
  loop(DataDecompFun, NextPid).

-spec loop(skel:data_decomp_fun(), pid()) -> 'eos'.
loop(DataDecompFun, NextPid) ->
  receive
    {data, _, _} = DataMessage ->
      PartitionMessages = DataDecompFun(DataMessage),
      Ref = make_ref(),
      sk_tracer:t(60, self(), {?MODULE, data}, [{ref, Ref}, {input, DataMessage}, {partitions, PartitionMessages}]),
      dispatch(Ref, length(PartitionMessages), PartitionMessages, NextPid),
      loop(DataDecompFun, NextPid);
    {system, eos} ->
      sk_tracer:t(75, self(), NextPid, {?MODULE, system}, [{msg, eos}]),
      NextPid ! {system, eos},
      eos
  end.

% We can guarantee to always have enough workers by the time we get to this method.
-spec dispatch(reference(), pos_integer(), [skel:data_message(),...], pid()) -> 'ok'.
dispatch(Ref, NPartitions, PartitionMessages, NextPid) ->
  dispatch(Ref, NPartitions, 1, PartitionMessages, NextPid).

-spec dispatch(reference(), pos_integer(), pos_integer(), [skel:data_message(),...], pid()) -> 'ok'.
dispatch(_Ref,_NPartitions, _Idx, [], _NextPid) ->
  ok;
dispatch(Ref, NPartitions, Idx, [PartitionMessage|PartitionMessages], NextPid) ->
  PartitionMessage1 = sk_data:push({decomp, Ref, Idx, NPartitions}, PartitionMessage),
  sk_tracer:t(50, self(), NextPid, {?MODULE, data}, [{partition, PartitionMessage1}]),
  NextPid ! PartitionMessage1,
  dispatch(Ref, NPartitions, Idx+1, PartitionMessages, NextPid).
