%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains `feedback` skeleton filter logic.
%%%
%%% The `feedback` skeleton contains an inner skeleton. Inputs are repeatedly
%%% sent through the inner skeleton until the developer-defined feedback
%%% function returns false for the input.
%%%
%%% The receiver receives inputs from the previous skeleton or the feedback 
%%% filter and sends them through the inner skeleton.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_feedback_receiver).

-export([
         start/4
        ]).

-define(counter, sk_feedback_bicounter).

-ifdef(TEST).
-compile(export_all).
-endif.

-spec start(reference(), pid(), pid(), pid()) -> 'eos'.
start(Ref, CounterPid, FilterPid, WorkerPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{reference, Ref},
                                             {counter_pid, CounterPid},
                                             {filter_pid, FilterPid},
                                             {worker_pid, WorkerPid}]),
  setup(Ref, self(), FilterPid),
  loop(false, CounterPid, WorkerPid).

-spec loop(boolean(), pid(), pid()) -> 'eos'.
loop(EosRecvd, CounterPid, WorkerPid) ->
  receive
    {data,_,_} = DataMessage ->
      case sk_data:peek(DataMessage) of
        {ok, feedback} -> from_feedback(DataMessage, CounterPid, WorkerPid);
        _              -> from_regular(DataMessage, CounterPid, WorkerPid)
      end,
      loop(EosRecvd, CounterPid, WorkerPid);
    {system, eos} ->
      ?counter:subscribe(CounterPid),
      loop(true, CounterPid, WorkerPid);
    {system, {counter, Counters, _}} ->
      case Counters of
        {0,0} ->
          WorkerPid ! {system, eos},
          eos;
        _ ->
          loop(EosRecvd, CounterPid, WorkerPid)
      end;
    {system, _} = SysMsg ->
      WorkerPid ! SysMsg,
      loop(EosRecvd, CounterPid, WorkerPid)
  end.


-spec setup(reference(), pid(), pid()) -> 'ok'.
setup(Ref, ReceiverPid, FilterPid) ->
  sk_tracer:t(75, self(), FilterPid, {?MODULE, system}, [{msg, feedback_setup}, {receiver_pid, ReceiverPid}, {reference, Ref}]),
  FilterPid ! {system, {feedback_setup, ReceiverPid, Ref}},
  receive
    {system, {feedback_reply, FilterPid, Ref}} ->
      ok
  end.

-spec from_feedback(skel:data_message(), pid(), pid()) -> ok.
from_feedback(DataMessage, CounterPid, WorkerPid) ->
  ?counter:cast(CounterPid, {incr, decr}),
  {feedback, DataMessage1} = sk_data:pop(DataMessage),
  sk_tracer:t(50, self(), WorkerPid, {?MODULE, data}, [{output, DataMessage1}]),
  WorkerPid ! DataMessage1,
  ok.

-spec from_regular(skel:data_message(), pid(), pid()) -> ok.
from_regular(DataMessage, CounterPid, WorkerPid) ->
  ?counter:cast(CounterPid, {incr, id}),
  sk_tracer:t(50, self(), WorkerPid, {?MODULE, data}, [{output, DataMessage}]),
  WorkerPid ! DataMessage,
  ok.

