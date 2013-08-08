%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains `feedback` skeleton filter logic.
%%%
%%% The `feedback` skeleton contains an inner skeleton. Inputs are repeatedly
%%% sent through the inner skeleton until the developer-defined feedback
%%% function returns false for the input.
%%%
%%% The filter is what sends the inputs back to the start of the inner skeleton
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_feedback_filter).

-export([
         start/4
        ]).

-define(counter, sk_feedback_bicounter).

-ifdef(TEST).
-compile(export_all).
-endif.

-spec start(skel:filter_fun(), reference(), pid(), pid()) -> 'eos'.
start(FilterFun, Ref, CounterPid, NextPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{reference, Ref},
                                             {counter_pid, CounterPid},
                                             {next_pid, NextPid}]),
  ReceiverPid = setup(self(), Ref),
  DataFilterFun = sk_data:filter_with(FilterFun),
  loop(DataFilterFun, CounterPid, ReceiverPid, NextPid).

-spec loop(skel:data_filter_fun(), pid(), pid(), pid()) -> 'eos'.
loop(DataFilterFun, CounterPid, ReceiverPid, NextPid) ->
  receive
    {data,_,_} = DataMessage ->
      case DataFilterFun(DataMessage) of
        true  -> feedback(DataMessage, CounterPid, ReceiverPid);
        false -> forward(DataMessage,  CounterPid, NextPid)
      end,
      loop(DataFilterFun, CounterPid, ReceiverPid, NextPid);
    {system, eos} ->
      sk_tracer:t(75, self(), NextPid, {?MODULE, system}, [{msg, eos}]),
      CounterPid ! {system, eos},
      NextPid ! {system, eos},
      eos
    end.

-spec setup(pid(), reference()) -> pid().
setup(FilterPid, Ref) ->
  receive
    {system, {feedback_setup, ReceiverPid, Ref}} ->
      sk_tracer:t(75, FilterPid, ReceiverPid, {?MODULE, system}, [{msg, feedback_reply}, {filter_pid, FilterPid}, {reference, Ref}]),
      ReceiverPid ! {system, {feedback_reply, FilterPid, Ref}},
      ReceiverPid
  end.

-spec feedback(skel:data_message(), pid(), pid()) -> ok.
feedback(DataMessage, CounterPid, Pid) ->
  ?counter:cast(CounterPid, {decr, incr}),
  DataMessage1 = sk_data:push(feedback, DataMessage),
  sk_tracer:t(50, self(), Pid, {?MODULE, data}, [{output, DataMessage1}]),
  Pid ! DataMessage1,
  ok.

-spec forward(skel:data_message(), pid(), pid()) -> ok.
forward(DataMessage, CounterPid, Pid) ->
  ?counter:cast(CounterPid, {decr, id}),
  sk_tracer:t(50, self(), Pid, {?MODULE, data}, [{output, DataMessage}]),
  Pid ! DataMessage,
  ok.
