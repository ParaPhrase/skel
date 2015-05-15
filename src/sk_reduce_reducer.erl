%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% 
%%% @doc This module contains Reduce skeleton reduce logic.
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

-include("skel.hrl").

-type maybe_data() :: unit | data_message().


-spec start(data_reduce_fun(), pid()) -> eos.
%% @doc Starts the reducer worker. The reducer worker recursively applies the 
%% developer-defined transformation `DataFun' to the input it receives.
start(DataFun, NextPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{next_pid, NextPid}]),
  loop(dict:new(), 0, DataFun, NextPid).

% Message Receiver Loop
% 1st Case: data message. Stores items in a dictionary until they can be reduced (i.e. on every second call, dict1 is emptied).
% 2nd Case: Occurs when you have an odd-length list as input.

-spec loop(dict:dict(), integer(), data_reduce_fun(), pid()) -> eos.
%% @doc The main message receiver loop. Recursively receives messages upon 
%% which, if said messages carry data, a reduction is attempted using 
%% `DataFun'. 
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

-spec store(reference(), dict:dict(), maybe_data()) -> dict:dict().
%% @doc Stores the given reference `Ref' and value `Value' in the dictionary 
%% `Dict'. Returns the resulting dictionary.
store(Ref, Dict, Value) ->
  dict:append(Ref, Value, Dict).

-spec maybe_reduce(reference(), integer(), pid(), data_reduce_fun(), dict:dict()) -> dict:dict().
%% @doc Attempts to find the reference `Ref' in the dictionary `Dict'. If 
%% found, a reduction shall be attempted. Otherwise, the dictionary is simply 
%% returned.
maybe_reduce(Ref, ReduceCount, NextPid, DataFun, Dict) ->
  case dict:find(Ref, Dict) of
    {ok, DMList}  -> reduce(Ref, ReduceCount, NextPid, DMList, DataFun, Dict);
    _             -> Dict
  end.

% The actual reduction function.
% Case 1: we are effectively empty (why would this happen?)
% Case 2 & 3: reached the end of the list/found an unpopulated node, consider the data message reduced.
% Case 4: Reference has two data message entries, which are then reduced.
% Deletes the reference from the dictionary, result is returned.

-spec reduce(reference(), integer(), pid(), [maybe_data(),...], data_reduce_fun(), dict:dict()) -> dict:dict().
%% @doc The reduction function. Given a list of length two containing specific 
%% data messages retreived from `Dict', all messages are reduced to a single 
%% message. Returns the (now-erased) dictionary.
reduce(Ref, ReduceCount, NextPid, [DM1, DM2] = DMList, DataFun, Dict) when length(DMList) == 2 ->
  case {DM1, DM2} of
    {unit, unit} -> 
      forward_unit(Ref, ReduceCount, NextPid);
    {unit, DM2}  -> 
      forward(Ref, ReduceCount, NextPid, DM2);
    {DM1,  unit} -> 
      forward(Ref, ReduceCount, NextPid, DM1);
    {DM1, DM2}   ->
      DM = DataFun(DM1, DM2),
      forward(Ref, ReduceCount, NextPid, DM)
  end,
  dict:erase(Ref, Dict);
reduce(_Ref, _ReduceCount, _NextPid, _DMList, _DataFun, Dict) ->
  Dict.

-spec forward(reference(), integer(), pid(), data_message()) -> ok.
%% @doc Formats the reduced message, then submits said message for sending. 
%% Adds reference and counter information to the message's identifiers.
forward(_Ref, ReduceCount, NextPid, DataMessage) when ReduceCount =< 0 ->
  forward(NextPid, DataMessage);
forward(Ref, ReduceCount, NextPid, DataMessage) ->
  DataMessage1 = sk_data:push({reduce, Ref, ReduceCount}, DataMessage),
  forward(NextPid, DataMessage1).

-spec forward(pid(), data_message()) -> ok.
%% @doc Sends the message to the process <tt>NextPid</tt>.
forward(NextPid, DataMessage) ->
  sk_tracer:t(50, self(), NextPid, {?MODULE, data}, [{message, DataMessage}]),
  NextPid ! DataMessage,
  ok.

-spec forward_unit(reference(), integer(), pid()) -> ok.
%% @doc Sends notification of double unit reduction to the process `NextPid'. 
%% No message formatting is required.
forward_unit(_Ref, ReduceCount, _NextPid) when ReduceCount =< 0 ->
  ok;
forward_unit(Ref, ReduceCount, NextPid) ->
  sk_tracer:t(75, self(), NextPid, {?MODULE, system}, [{message, reduce_unit}, {ref, Ref}, {reduce_count, ReduceCount}]),
  NextPid ! {system, {reduce_unit, Ref, ReduceCount}},
  ok.
