%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%%
%%% @doc This module contains the collector logic of a Farm skeleton.
%%%
%%% A task farm has the most basic kind of stream parallelism - inputs are
%%% sent to one of `n' replicas of the inner skeleton for processing.
%%%
%%% The collector takes inputs off the inner-skeletons' output streams, sending
%%% them out on the farm skeleton's output stream. It does not preserve 
%%% ordering.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_farm_collector).

-export([
         start/2
        ]).

-include("skel.hrl").

-spec start(pos_integer(), pid()) -> 'eos'.
%% @doc Initialises the collector; forwards any and all output from the inner-
%% workflow to the sink process at `NextPid'.
start(NWorkers, NextPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{num_workers, NWorkers}, {next_pid, NextPid}]),
  loop(NWorkers, NextPid).

-spec loop(pos_integer(), pid()) -> 'eos'.
%% @doc Worker-function for {@link start/2}. Recursively receives, and 
%% forwards, any output messages from the inner-workflow. Halts when the `eos' 
%% system message is received, and only one active worker process remains.
loop(NWorkers, NextPid) ->
  receive
    {data, _, _} = DataMessage ->
	  io:format("Collected data message ~p~n",[DataMessage]),
	  sk_tracer:t(50, self(), NextPid, {?MODULE, data}, [{input, DataMessage}]),
	  NextPid ! DataMessage,
	  loop(NWorkers, NextPid);
    {system, eos} when NWorkers =< 1 ->
      sk_tracer:t(75, self(), NextPid, {?MODULE, system}, [{msg, eos}, {remaining, 0}]),
      NextPid ! {system, eos},
      eos;
    {system, eos} ->
      sk_tracer:t(85, self(), {?MODULE, system}, [{msg, eos}, {remaining, NWorkers-1}]),
      loop(NWorkers-1, NextPid)
  end.

