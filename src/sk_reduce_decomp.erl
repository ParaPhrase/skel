%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% 
%%% @doc This module contains Reduce skeleton decomp logic.
%%%
%%% The decomp process splits an input into many parts using a developer-
%%% defined function, then hands it on to the tree of reducer processes to be
%%% reduced.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_reduce_decomp).

-export([
         start/3
        ]).

-include("skel.hrl").

-type pid_pools() :: dict().

-spec start(decomp_fun(), reduce_fun(), pid()) -> eos.
%% @doc Starts the reduce process. Takes the developer-defined reduction and 
%% decompostion functions, `Reduce' and `Decomp', produces a tree of processes 
%% to handle reduction, and recursively reduces input.
start(Decomp, Reduce, NextPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{next_pid, NextPid}]),
  DataDecompFun = sk_data:decomp_by(Decomp),
  DataReduceFun = sk_data:reduce_with(Reduce),
  PidPools = dict:new(),
  NewReducer = proc_lib:spawn(sk_reduce_reducer, start, [DataReduceFun, NextPid]),
  PidPools1 = dict:store(0, [NewReducer], PidPools),
  loop(DataDecompFun, DataReduceFun, NextPid, PidPools1).

-spec loop(data_decomp_fun(), data_reduce_fun(), pid(), pid_pools()) -> eos.
%% @doc Recursively receives and reduces input. In charge of dispatching work 
%% and input to reducers.
loop(DataDecompFun, DataReduceFun, NextPid, PidPools) ->
  receive
    {data, _, _} = DataMessage ->
      PartitionMessages = DataDecompFun(DataMessage),
      PidPools1 = start_reducers(length(PartitionMessages), DataReduceFun, NextPid, PidPools),
      dispatch(PartitionMessages, NextPid, PidPools1),
      loop(DataDecompFun, DataReduceFun, NextPid, PidPools1);
    {system, eos} ->
      stop_reducers(PidPools)
  end.

-spec start_reducers(pos_integer(), data_reduce_fun(), pid(), pid_pools()) -> pid_pools().
%% @doc Recursively produces and starts reducers. Calculates the total number 
%% of reducers needed based on the number of partitions `NPartitions' 
%% specified. If this number has not been reached, as determined by 
%% {@link top_pool/1}, create a new pool.
start_reducers(NPartitions, DataReduceFun, NextPid, PidPools) ->
  TopPool = top_pool(PidPools),
  RequiredPool = ceiling(log2(NPartitions/2)),
  if
    TopPool < RequiredPool ->
      TopPoolPids = dict:fetch(TopPool, PidPools),
      NewPids = [proc_lib:spawn(sk_reduce_reducer, start, [DataReduceFun, NextPoolPid])
                 || NextPoolPid <- TopPoolPids, _ <- [1,2]],
      PidPools1 = dict:store(TopPool+1, NewPids, PidPools),
      start_reducers(NPartitions, DataReduceFun, NextPid, PidPools1);
    true -> PidPools
  end.

-spec dispatch([data_message(),...], pid(), pid_pools()) -> ok.
%% @doc Sends all input to reducers stored in `PidPools'.
dispatch([DataMessage] = PartitionMessages, NextPid, _PidPools) when length(PartitionMessages) == 1 ->
  sk_tracer:t(75, self(), NextPid, {?MODULE, data}, [{data_message, DataMessage}]),
  NextPid ! DataMessage,
  ok;
dispatch(PartitionMessages, _NextPid, PidPools) ->
  NPartitions = length(PartitionMessages),
  RequiredPool = ceiling(log2(NPartitions/2)),
  RequiredPoolPids = dict:fetch(RequiredPool, PidPools),
  ReduceCount = ceiling(log2(NPartitions)),
  Ref = make_ref(),
  dispatch(Ref, ReduceCount, PartitionMessages, RequiredPoolPids ++ RequiredPoolPids).

-spec dispatch(reference(), pos_integer(), [data_message()], [pid()]) -> ok.
%% @doc Recursive worker for {@link dispatch/3}. Updates messages' 
%% identifiers, and sends messages to reducers.
dispatch(_Ref, _ReduceCount, [], []) ->
  ok;
dispatch(Ref, ReduceCount, []=DataMessages, [NextPid|NextPids]) ->
  sk_tracer:t(75, self(), NextPid, {?MODULE, system}, [{message, reduce_unit}, {ref, Ref}, {reduce_count, ReduceCount}]),
  NextPid ! {system, {reduce_unit, Ref, ReduceCount}},
  dispatch(Ref, ReduceCount, DataMessages, NextPids);
dispatch(Ref, ReduceCount, [DataMessage|DataMessages], [NextPid|NextPids]) ->
  DataMessage1 = sk_data:push({reduce, Ref, ReduceCount}, DataMessage),
  sk_tracer:t(50, self(), NextPid, {?MODULE, data}, [{partition, DataMessage1}]),
  NextPid ! DataMessage1,
  dispatch(Ref, ReduceCount, DataMessages, NextPids).

-spec stop_reducers(pid_pools()) -> eos.
%% @doc Sends the halt command to all reducers.
stop_reducers(PidPools) ->
  TopPoolPids = dict:fetch(top_pool(PidPools), PidPools),
  sk_utils:stop_workers(?MODULE, TopPoolPids ++ TopPoolPids).

-spec top_pool(pid_pools()) -> number().
%% @doc Finds the index of the last added PidPool.
top_pool(PidPools) ->
  Pools = dict:fetch_keys(PidPools),
  lists:max(Pools).

-spec log2(number()) -> number().
%% @doc Finds the binary logarithm of `X'.
log2(X) ->
  math:log(X) / math:log(2).

-spec ceiling(number()) -> integer().
%% @doc Rounds `X' up to the nearest integer.
ceiling(X) ->
  case trunc(X) of
      Y when Y < X -> Y + 1
    ; Z -> Z
  end.
