%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% 
%%% @doc This module contains what happens in the decomposition process of a 
%%% Cluster wrapper. 
%%%
%%% The cluster wrapper acts in a similar manner to the Map skeleton, but 
%%% allows the developer to customise the decomposition and recomposition 
%%% functions used. Inputs are decomposed according to the developer-defined 
%%% decomposition function, and then passed to the worker process running the 
%%% inner-workflow.
%%%
%%% The decomposition process divides each input according to a developer-
%%% defined function. This function should compliment the similarly developer-
%%% defined recomposition function.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_cluster_decomp).

-export([
         start/2
        ]).

-include("skel.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-spec start(decomp_fun(), pid()) -> 'eos'.
%% @doc Initialises the decomposition process.
%% 
%% The decomposition process listens for input, dividing them into partite 
%% elements upon receipt. This decomposition is powered by the decomposition 
%% function given by `Decomp'.
start(Decomp, NextPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{next_pid, NextPid}]),
  DataDecompFun = sk_data:decomp_by(Decomp),
  loop(DataDecompFun, NextPid).

-spec loop(data_decomp_fun(), pid()) -> 'eos'.
%% @doc Worker function for {@link start/2}; recursively receives input, which is then decomposed using the function under `DataDecompFun'. These decomposed elements are then dispatched to the worker processes.
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

-spec dispatch(reference(), pos_integer(), [data_message(),...], pid()) -> 'ok'.
%% @doc Partite elements listed in `PartitionMessages' are formatted so that 
%% they may be recomposed later. Each are then sent to the worker process at 
%% `NextPid'. 
dispatch(Ref, NPartitions, PartitionMessages, NextPid) ->
  dispatch(Ref, NPartitions, 1, PartitionMessages, NextPid).

-spec dispatch(reference(), pos_integer(), pos_integer(), [data_message(),...], pid()) -> 'ok'.
%% @doc Worker function for {@link dispatch/4}; recursively dispatches each data message to `NextPid'.
dispatch(_Ref,_NPartitions, _Idx, [], _NextPid) ->
  ok;
dispatch(Ref, NPartitions, Idx, [PartitionMessage|PartitionMessages], NextPid) ->
  PartitionMessage1 = sk_data:push({decomp, Ref, Idx, NPartitions}, PartitionMessage),
  sk_tracer:t(50, self(), NextPid, {?MODULE, data}, [{partition, PartitionMessage1}]),
  NextPid ! PartitionMessage1,
  dispatch(Ref, NPartitions, Idx+1, PartitionMessages, NextPid).
