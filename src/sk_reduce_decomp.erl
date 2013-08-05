%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains `reduce` skeleton decomp logic.
%%%
%%% The decomp process splits an input into many parts, (using a developer-
%%% defined function), then hands it on to the tree of reducer processes to be
%%% reduced.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_reduce_decomp).

-export([
         start/3
        ]).


-ifdef(TEST).
-compile(export_all).
-endif.

% This type represents an ordered set of sets of pids.
% The first pool will have 2^n pids, the next 2^n-1 and so on until the last pool just has 1 pid.
% We keep references to all of them, because of the complex dispatch rules.
-type pid_pools() :: dict().

-spec start(skel:decomp_fun(), skel:reduce_fun(), pid()) -> eos.
start(Decomp, Reduce, NextPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{next_pid, NextPid}]),
  DataDecompFun = sk_data:decomp_by(Decomp),
  DataReduceFun = sk_data:reduce_with(Reduce),
  PidPools = dict:new(),
  NewReducer = spawn(sk_reduce_reducer, start, [DataReduceFun, NextPid]),
  PidPools1 = dict:store(0, [NewReducer], PidPools),
  loop(DataDecompFun, DataReduceFun, NextPid, PidPools1).

-spec loop(skel:data_decomp_fun(), skel:data_reduce_fun(), pid(), pid_pools()) -> eos.
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

-spec start_reducers(pos_integer(), skel:data_reduce_fun(), pid(), pid_pools()) -> pid_pools().
start_reducers(NPartitions, DataReduceFun, NextPid, PidPools) ->
  TopPool = top_pool(PidPools),
  RequiredPool = ceiling(log2(NPartitions/2)),
  if
    TopPool < RequiredPool ->
      TopPoolPids = dict:fetch(TopPool, PidPools),
      NewPids = [spawn(sk_reduce_reducer, start, [DataReduceFun, NextPoolPid]) || NextPoolPid <- TopPoolPids, _ <- [1,2]],
      PidPools1 = dict:store(TopPool+1, NewPids, PidPools),
      start_reducers(NPartitions, DataReduceFun, NextPid, PidPools1);
    true -> PidPools
  end.

-spec dispatch([skel:data_message(),...], pid(), pid_pools()) -> ok.
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

-spec dispatch(reference(), pos_integer(), [skel:data_message()], [pid()]) -> ok.
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
stop_reducers(PidPools) ->
  TopPoolPids = dict:fetch(top_pool(PidPools), PidPools),
  sk_utils:stop_workers(?MODULE, TopPoolPids ++ TopPoolPids).

-spec top_pool(pid_pools()) -> number().
top_pool(PidPools) ->
  Pools = dict:fetch_keys(PidPools),
  lists:max(Pools).

-spec log2(number()) -> number().
log2(X) ->
  math:log(X) / math:log(2).

-spec ceiling(number()) -> integer().
ceiling(X) ->
  case trunc(X) of
      Y when Y < X -> Y + 1
    ; Z -> Z
  end.
