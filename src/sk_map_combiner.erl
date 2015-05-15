%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% 
%%% @doc This module contains the Map skeleton combiner logic.
%%% 
%%% The Map skeleton is a parallel map. The skeleton applies a given function 
%%% to the elements within one or more lists.
%%% 
%%% The combiner receives each partite element of each input following the 
%%% original element's application to the given workflow. This module collects 
%%% each resulting partite element of all inputs and restores the structure of 
%%% the original inputs.
%%% 
%%% Similar to {@link sk_map_partitioner}, this module supports both the 
%%% automatic creation of workers, and the ability to define the number used.
%%% 
%%% @end
%%%----------------------------------------------------------------------------

-module(sk_map_combiner).

-export([start/1, start/2]).

-include("skel.hrl").

-spec start(pid()) -> 'eos'.
%% @doc Initialises the recomposition process for when the number of workers 
%% is <em>not</em> set by the developer. 
%% 
%% Recomposition consists of rebuilding 
%% a list from its elements, in the correct order. For each set of elements, a 
%% {@link data_message()} is produced and sent to the process given by 
%% `NextPid'.
start(NextPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{next_pid, NextPid}]),
  loop(auto, 0, 0, dict:new(), recomp_with(), NextPid).


-spec start(pid(), pos_integer()) -> 'eos'.
%% @doc Initialises the recomposition process for when the number of workers 
%% <em>is</em> set by the developer. 
%% 
%% Recomposition similarly consists of rebuilding a list from its elements, in 
%% the correct order. For each set of elements a {@link data_message()} is 
%% produced and sent to the process given by `NextPid'.
start(NextPid, NWorkers) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{next_pid, NextPid}]),
  loop(man, NWorkers, 0, dict:new(), recomp_with(), NextPid).


-spec loop(atom(), non_neg_integer(), non_neg_integer(), dict:dict(), data_recomp_fun(), pid()) -> 'eos'.
%% @doc Recursively receives and stores messages until groups of said messages 
%% may be recomposed and sent. Serves to stop all processes once all inputs 
%% have been processed.
%% 
%% The first clause is used when the number of workers is left unspecified by 
%% the developer. The second, where it is specified.
loop(auto, TotWorkers, DeadWorkers, Dict, DataCombinerFun, NextPid) ->
  receive
    {data, _, _} = PartitionMessage ->
      {{decomp, Ref, Idx, NPartitions}, PartitionMessage1} = sk_data:pop(PartitionMessage),
      Dict1 = store(Ref, Idx, NPartitions, PartitionMessage1, Dict),
      Dict2 = combine_and_forward(Ref, Dict1, DataCombinerFun, NextPid),
      TotWorkers1 = new_total_workers(TotWorkers, NPartitions),
      loop(auto, TotWorkers1, DeadWorkers, Dict2, DataCombinerFun, NextPid);
    {system, eos} when DeadWorkers+1 >= TotWorkers ->
      sk_tracer:t(75, self(), NextPid, {?MODULE, system}, [{msg, eos}, {total, TotWorkers}, {dead, DeadWorkers+1}]),
      NextPid ! {system, eos},
      eos;
    {system, eos} ->
      sk_tracer:t(85, self(), {?MODULE, system}, [{msg, eos}, {total, TotWorkers}, {dead, DeadWorkers+1}]),
      loop(auto, TotWorkers, DeadWorkers+1, Dict, DataCombinerFun, NextPid)
  end;
loop(man, TotWorkers, DeadWorkers, Dict, DataCombinerFun, NextPid) ->
  receive
    {data, _, _} = PartitionMessage ->
      {{decomp, Ref, Idx, NPartitions}, PartitionMessage1} = sk_data:pop(PartitionMessage),
      Dict1 = store(Ref, Idx, NPartitions, PartitionMessage1, Dict),
      Dict2 = combine_and_forward(Ref, Dict1, DataCombinerFun, NextPid),
      loop(man, TotWorkers, DeadWorkers, Dict2, DataCombinerFun, NextPid);
    {system, eos} when DeadWorkers+1 >= TotWorkers ->
      sk_tracer:t(75, self(), NextPid, {?MODULE, system}, [{msg, eos}, {total, TotWorkers}, {dead, DeadWorkers+1}]),
      NextPid ! {system, eos},
      eos;
    {system, eos} ->
      sk_tracer:t(85, self(), {?MODULE, system}, [{msg, eos}, {total, TotWorkers}, {dead, DeadWorkers+1}]),
      loop(man, TotWorkers, DeadWorkers+1, Dict, DataCombinerFun, NextPid)
  end.


-spec recomp_with() -> data_recomp_fun().
%% @doc Provides the recomposition function and means to merge many inputs 
%% into one. This appends each individual `DataMessage', in order, to a list. 
%% This list is wrapped in a single {@link data_message()}.
recomp_with() ->
  fun([{data, _, Ids}|_] = DataMessages) ->
    {data, [Value || {_, Value, _} <- DataMessages], Ids}
  end.


-spec new_total_workers(non_neg_integer(), non_neg_integer()) -> non_neg_integer().
%% @doc Returns the total number of workers used by the skeleton. Employed
%%  when the number of workers is automatically determined. 
new_total_workers(TotWorkers, NPartitions) when NPartitions > TotWorkers ->
  NPartitions;
new_total_workers(TotWorkers, _NPartitions) ->
  TotWorkers.


-spec store(reference(), pos_integer(), pos_integer(), data_message(), dict:dict()) -> dict:dict().
%% @doc Stores in a dictionary the total number of partitions, `NPartitions', 
%% expected; all messages heretofore received; and the number said received 
%% messages, for the original input under the reference given by `Ref'. The 
%% updated dictionary, using `Dict' as a base, is returned.
store(Ref, Idx, NPartitions, PartitionMessage, Dict) ->
  Dict1 = dict:store({Ref, expecting}, NPartitions, Dict),
  Dict2 = dict:store({Ref, Idx}, PartitionMessage, Dict1),
  dict:update_counter({Ref, received}, 1, Dict2).


-spec combine_and_forward(reference(), dict:dict(), data_recomp_fun(), pid()) -> dict:dict().
%% @doc Attempts to find the reference as given by `Ref' in the specified 
%% dictionary.
%% 
%% If said reference is found, {@link combine_and_forward/5} is used to 
%% attempt a recomposition of the partite elements stored as messages in 
%% `Dict'.
combine_and_forward(Ref, Dict, DataCombinerFun, NextPid) ->
  case dict:find({Ref, expecting}, Dict) of
    error             -> Dict;
    {ok, NPartitions} -> combine_and_forward(Ref, Dict, NPartitions, DataCombinerFun, NextPid)
  end.


-spec combine_and_forward(reference(), dict:dict(), pos_integer(), data_recomp_fun(), pid()) -> dict:dict().
%% @doc Inner-function for {@link combine_and_forward/4} that attempts to 
%% restore a decomposed list from parts in a dictionary `Dict', whose 
%% reference is given by `Ref'.
%% 
%% If all decomposed elements can be found, `combine_and_forward/5' retrieves 
%% them and applies the recomposition function under `DataCombinerFun'. The 
%% resulting data message is sent to the process represented by `NextPid', 
%% those messages deleted from the dictionary, and the dictionary returned.
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


-spec fetch_partitions(reference(), non_neg_integer(), dict:dict(), [any()]) -> [any()].
%% @doc Returns a list of all data messages in the given dictionary, whose 
%% reference is `Ref'.
fetch_partitions(_Ref, 0, _Dict, Acc) ->
  Acc;
fetch_partitions(Ref, NPartitions, Dict, Acc) ->
  {ok, Piece} = dict:find({Ref, NPartitions}, Dict),
  fetch_partitions(Ref, NPartitions-1, Dict, [Piece|Acc]).


-spec purge_partitions(reference(), non_neg_integer(), dict:dict()) -> dict:dict().
%% @doc Recursively removes all entries with `Ref' as their reference in the 
%% given dictionary.
purge_partitions(Ref, 0, Dict) ->
  Dict1 = dict:erase({Ref, expecting}, Dict),
  Dict2 = dict:erase({Ref, received}, Dict1),
  Dict2;
purge_partitions(Ref, NPartitions, Dict) ->
  Dict1 = dict:erase({Ref, NPartitions}, Dict),
  purge_partitions(Ref, NPartitions-1, Dict1).
