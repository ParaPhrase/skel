%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% 
%%% @doc This module contains what happens in the recomposition process of a 
%%% Cluster wrapper. 
%%%
%%% The cluster wrapper acts in a similar manner to the Map skeleton, but 
%%% allows the developer to customise the decomposition and recomposition 
%%% functions used. Inputs are decomposed according to the developer-defined 
%%% decomposition function, and then passed to the worker process running the 
%%% inner-workflow.
%%%
%%% The recomposition process recombines partite elements of each input 
%%% according to a developer-defined function. This function should compliment 
%%% the similarly developer-defined decomposition function.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_cluster_recomp).

-export([
         start/2
        ]).

-include("skel.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-spec start(recomp_fun(), pid()) -> 'eos'.
%% @doc Initialises the recomposition process.
%% 
%% The recomposition process listens for data messages, combining all messages 
%% that are a partite element of the same original input for all inputs. this 
%% recomposition is powered by the recomposition function given by `Recomp'.
start(Recomp, NextPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{next_pid, NextPid}]),
  DataRecompFun = sk_data:recomp_with(Recomp),
  loop(dict:new(), DataRecompFun, NextPid).

-spec loop(dict:dict(), data_recomp_fun(), pid()) -> 'eos'.
%% @doc Worker function for {@link start/2}; recursively receives and combines 
%% messages using the recomposition function under `DataRecompFun'. Once all 
%% partite elements for each original input have been received and merged, the 
%% recomposed message is forwarded to `NextPid'.
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

-spec store(reference(), pos_integer(), pos_integer(), data_message(), dict:dict()) -> dict:dict().
%% @doc Facilitates the storing of all partite and semi-combined messages, for each input. 
%% 
%% The total number of partite elements expected, the partite element data 
%% messages themselves, and the number of these received, are stored for each
%% of the original inputs -- as grouped by `Ref' -- are stored. The updated 
%% dictionary is returned.
store(Ref, Idx, NPartitions, PartitionMessage, Dict) ->
  Dict1 = dict:store({Ref, expecting}, NPartitions, Dict),
  Dict2 = dict:store({Ref, Idx}, PartitionMessage, Dict1),
  dict:update_counter({Ref, received}, 1, Dict2).

-spec combine_and_forward(reference(), dict:dict(), data_recomp_fun(), pid()) -> dict:dict().
%% @doc Attempts to find the grouping reference under `Ref' in the given 
%% dictionary. If that reference is found, all message parts are combined 
%% using the recomposition function given under `DataCombinerFun'.
combine_and_forward(Ref, Dict, DataCombinerFun, NextPid) ->
  case dict:find({Ref, expecting}, Dict) of
    error             -> Dict;
    {ok, NPartitions} -> combine_and_forward(Ref, Dict, NPartitions, DataCombinerFun, NextPid)
  end.

-spec combine_and_forward(reference(), dict:dict(), pos_integer(), data_recomp_fun(), pid()) -> dict:dict().
%% @doc Worker function for {@link combine_and_forward/4}; attempts to 
%% recompose a partite elements for a given original input, as indicated by 
%% `Ref'. Should all partite elements be stored in the dictionary, they are 
%% retrieved and recomposed. The result of which is sent to `NextPid', and 
%% those partite elements removed from the dictionary. The updated dictionary 
%% is returned.
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
%% @doc Retrieves and returns a list of all entries with the same reference in 
%% the specified dictionary.
fetch_partitions(_Ref, 0, _Dict, Acc) ->
  Acc;
fetch_partitions(Ref, NPartitions, Dict, Acc) ->
  {ok, Piece} = dict:find({Ref, NPartitions}, Dict),
  fetch_partitions(Ref, NPartitions-1, Dict, [Piece|Acc]).

-spec purge_partitions(reference(), non_neg_integer(), dict:dict()) -> dict:dict().
%% @doc Recursively removes all entries with the same reference in a given 
%% dictionary. 
purge_partitions(Ref, 0, Dict) ->
  Dict1 = dict:erase({Ref, expecting}, Dict),
  Dict2 = dict:erase({Ref, received}, Dict1),
  Dict2;
purge_partitions(Ref, NPartitions, Dict) ->
  Dict1 = dict:erase({Ref, NPartitions}, Dict),
  purge_partitions(Ref, NPartitions-1, Dict1).
