%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains `reduce` skeleton reduce logic.
%%%
%%% The reduce process takes two inputs, then applies the developer-defined
%%% reduce function to them, before forwarding on the results to the next step
%%% in the tree of reducers.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_reduce_reducer).

-export([
         start/2
        ]).


-ifdef(TEST).
-compile(export_all).
-endif.

-type maybe_data() :: unit | skel:data_message().

-spec start(skel:data_reduce_fun(), pid()) -> eos.
start(DataFun, NextPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{next_pid, NextPid}]),
  loop(dict:new(), 0, DataFun, NextPid).

-spec loop(dict(), integer(), skel:data_reduce_fun(), pid()) -> eos.
loop(Dict, EOSRecvd, DataFun, NextPid) ->
  receive
    {data, _, _} = DataMessage ->
      {{reduce, Reference, ReduceCount}, DataMessage1} = sk_data:pop(DataMessage),
      Dict1 = store(Reference, Dict, DataMessage1),
      Dict2 = maybe_reduce(Reference, ReduceCount-1, NextPid, DataFun, Dict1),
      loop(Dict2, EOSRecvd, DataFun, NextPid);
    {system, {reduce_unit, Reference, ReduceCount}} ->
      Dict1 = store(Reference, Dict, unit),
      Dict2 = maybe_reduce(Reference, ReduceCount-1, NextPid, DataFun, Dict1),
      loop(Dict2, EOSRecvd, DataFun, NextPid);
    {system, eos} when EOSRecvd >= 1 ->
      sk_tracer:t(75, self(), NextPid, {?MODULE, system}, [{msg, eos}]),
      NextPid ! {system, eos},
      eos;
    {system, eos} ->
      sk_tracer:t(85, self(), {?MODULE, system}, [{msg, eos}]),
      loop(Dict, EOSRecvd+1, DataFun, NextPid)
  end.

-spec store(reference(), dict(), maybe_data()) -> dict().
store(Ref, Dict, Value) ->
  dict:append(Ref, Value, Dict).

-spec maybe_reduce(reference(), integer(), pid(), skel:data_reduce_fun(), dict()) -> dict().
maybe_reduce(Ref, ReduceCount, NextPid, DataFun, Dict) ->
  case dict:find(Ref, Dict) of
    {ok, DMList}  -> reduce(Ref, ReduceCount, NextPid, DMList, DataFun, Dict);
    _             -> Dict
  end.

-spec reduce(reference(), integer(), pid(), [maybe_data(),...], skel:data_reduce_fun(), dict()) -> dict().
reduce(Ref, ReduceCount, NextPid, [DM1, DM2] = DMList, DataFun, Dict) when length(DMList) == 2 ->
  case {DM1, DM2} of
    {unit, unit} -> forward_unit(Ref, ReduceCount, NextPid);
    {unit, DM2}  -> forward(Ref, ReduceCount, NextPid, DM2);
    {DM1,  unit} -> forward(Ref, ReduceCount, NextPid, DM1);
    {DM1, DM2}   ->
      DM = DataFun(DM1, DM2),
      forward(Ref, ReduceCount, NextPid, DM)
  end,
  dict:erase(Ref, Dict);
reduce(_Ref, _ReduceCount, _NextPid, _DMList, _DataFun, Dict) ->
  Dict.

-spec forward(reference(), integer(), pid(), skel:data_message()) -> ok.
forward(_Ref, ReduceCount, NextPid, DataMessage) when ReduceCount =< 0 ->
  forward(NextPid, DataMessage);
forward(Ref, ReduceCount, NextPid, DataMessage) ->
  DataMessage1 = sk_data:push({reduce, Ref, ReduceCount}, DataMessage),
  forward(NextPid, DataMessage1).

-spec forward(pid(), skel:data_message()) -> ok.
forward(NextPid, DataMessage) ->
  sk_tracer:t(50, self(), NextPid, {?MODULE, data}, [{message, DataMessage}]),
  NextPid ! DataMessage,
  ok.

-spec forward_unit(reference(), integer(), pid()) -> ok.
forward_unit(_Ref, ReduceCount, _NextPid) when ReduceCount =< 0 ->
  ok;
forward_unit(Ref, ReduceCount, NextPid) ->
  sk_tracer:t(75, self(), NextPid, {?MODULE, system}, [{message, reduce_unit}, {ref, Ref}, {reduce_count, ReduceCount}]),
  NextPid ! {system, {reduce_unit, Ref, ReduceCount}},
  ok.
