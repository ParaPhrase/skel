%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% 
%%% @doc This module contains Feedback skeleton filter logic.
%%%
%%% The Feedback skeleton repeatedly passes output from its inner-workflow 
%%% back into said workflow until a given constraint-checking function fails.
%%%
%%% The receiver receives inputs from the previous skeleton or the feedback 
%%% filter and sends them through the inner-workflow.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_feedback_receiver).

-export([
         start/4
        ]).

-include("skel.hrl").

-define(counter, sk_feedback_bicounter).

-spec start(reference(), pid(), pid(), pid()) -> 'eos'.
%% @doc Begins the receiver, taking recycled and non-recycled input and 
%% passing them to a worker for application to the inner-workflow. 
start(Ref, CounterPid, FilterPid, WorkerPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{reference, Ref},
                                             {counter_pid, CounterPid},
                                             {filter_pid, FilterPid},
                                             {worker_pid, WorkerPid}]),
  setup(Ref, self(), FilterPid),
  loop(false, CounterPid, WorkerPid).


% Handles messages from the previous skeleton(?) or the feedback filter.
% case 1: std. data message. Looks at first identifier in message. If not empty: decrease the counter; otherwise do nothing. Then passes the message on to the worker.
% case 2: end of stream. Receiver subscribes to the counter. So that it can find out next time it receives one if all inputs have left.
% case 3: counter. Continues looping until counter = {0,0} at which point you halt.
% case 4: system message. Pass it along.
-spec loop(boolean(), pid(), pid()) -> 'eos'.
%% @doc Inner-function for {@link start/4}; recursively receives input as data %% messages, dealing with each accordingly.
%% 
%% A regular data message handling input is managed as to whether it has 
%% already passed through the inner-workflow, or is a new input. A counter 
%% data message may also be received, assisting with termination.
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
%% @doc Associates the current receiver process with the constraint-checking 
%% filter process given by `FilterPid'.
setup(Ref, ReceiverPid, FilterPid) ->
  sk_tracer:t(75, self(), FilterPid, {?MODULE, system}, [{msg, feedback_setup}, {receiver_pid, ReceiverPid}, {reference, Ref}]),
  FilterPid ! {system, {feedback_setup, ReceiverPid, Ref}},
  receive
    {system, {feedback_reply, FilterPid, Ref}} ->
      ok
  end.

-spec from_feedback(data_message(), pid(), pid()) -> ok.
%% @doc Re-formats a former-output message under `DataMessage', updates the 
%% counter, and passes the message to the worker process at `WorkerPid'.
from_feedback(DataMessage, CounterPid, WorkerPid) ->
  ?counter:cast(CounterPid, {incr, decr}),
  {feedback, DataMessage1} = sk_data:pop(DataMessage),
  sk_tracer:t(50, self(), WorkerPid, {?MODULE, data}, [{output, DataMessage1}]),
  WorkerPid ! DataMessage1,
  ok.

-spec from_regular(data_message(), pid(), pid()) -> ok.
%% @doc Forwards a new input message under `DataMessage', updates the counter, 
%% and passes the message onwards to the worker process at `WorkerPid'.
from_regular(DataMessage, CounterPid, WorkerPid) ->
  ?counter:cast(CounterPid, {incr, id}),
  sk_tracer:t(50, self(), WorkerPid, {?MODULE, data}, [{output, DataMessage}]),
  WorkerPid ! DataMessage,
  ok.

