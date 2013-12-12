%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% 
%%% @doc This module contains 'feedback' skeleton filter logic.
%%%
%%% The Feedback skeleton repeatedly passes output from its inner-workflow 
%%% back into said workflow until a given constraint-checking function fails.
%%%
%%% The filter sends the inputs back to the start of the inner skeleton should 
%%% they pass a given condition; forwarding them onwards otherwise.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_feedback_filter).

-export([
         start/4
        ]).

-include("skel.hrl").

-define(counter, sk_feedback_bicounter).

-spec start(filter_fun(), reference(), pid(), pid()) -> 'eos'.
%% @doc Initialises the constraint-checking filter process.
%% 
%% The developer-defined function represented by `FilterFun' serves to check 
%% the desired constraint. A reference under `Ref' serves to group inputs, 
%% workers and administrative processes to avoid conflicts with multiple 
%% feedback skeletons. The counter process, keeping track of how many inputs 
%% are currently passing through both the inner-workflow and the filter 
%% process, and sink Pids are provided under `CounterPid' and `NextPid' 
%% respectively.
start(FilterFun, Ref, CounterPid, NextPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{reference, Ref},
                                             {counter_pid, CounterPid},
                                             {next_pid, NextPid}]),
  ReceiverPid = setup(self(), Ref),
  DataFilterFun = sk_data:filter_with(FilterFun),
  loop(DataFilterFun, CounterPid, ReceiverPid, NextPid).

-spec loop(data_filter_fun(), pid(), pid(), pid()) -> 'eos'.
%% @doc Recursively receives messages from the worker process applying the 
%% inner-workflow. Messages are checked by the constraint function under 
%% `DataFilterFun', then passed back into the the inner-workflow or forwarded 
%% onwards to the process given by `NextPid'.
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
%% @doc Acknowledges the registration request of a receiver process, as described in {@link sk_feedback_receiver}.
setup(FilterPid, Ref) ->
  receive
    {system, {feedback_setup, ReceiverPid, Ref}} ->
      sk_tracer:t(75, FilterPid, ReceiverPid, {?MODULE, system}, [{msg, feedback_reply}, {filter_pid, FilterPid}, {reference, Ref}]),
      ReceiverPid ! {system, {feedback_reply, FilterPid, Ref}},
      ReceiverPid
  end.

% counter: {no. of inputs in inner skeleton, no. of inputs in feedback loop}
-spec feedback(data_message(), pid(), pid()) -> ok.
%% @doc Inputs passing the constraint-checking function are passed here for processing.
%% 
%% This function updates the counter at `CounterPid', and reformats the input 
%% message given by `DataMessage' for the inner-workflow. The now-reformatted 
%% message is sent to the receiver process at `Pid'.
feedback(DataMessage, CounterPid, Pid) ->
  ?counter:cast(CounterPid, {decr, incr}),
  DataMessage1 = sk_data:push(feedback, DataMessage),
  sk_tracer:t(50, self(), Pid, {?MODULE, data}, [{output, DataMessage1}]),
  Pid ! DataMessage1,
  ok.

-spec forward(data_message(), pid(), pid()) -> ok.
%% @doc New inputs are passed here for processing.
%% 
%% This function simply passes the message under `DataMessage' onto the sink 
%% process at `Pid' as output. The counter located at `CounterPid' is updated 
%% to reflect this.
forward(DataMessage, CounterPid, Pid) ->
  ?counter:cast(CounterPid, {decr, id}),
  sk_tracer:t(50, self(), Pid, {?MODULE, data}, [{output, DataMessage}]),
  Pid ! DataMessage,
  ok.
