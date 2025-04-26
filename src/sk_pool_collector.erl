%%%----------------------------------------------------------------------------
%%% @author Ramsay Taylor <r.g.taylor@sheffield.ac.uk>
%%%
%%% @doc This module contains the collector for the pool skeleton.
%%%
%%% 
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_pool_collector).

-export([
         start/3
        ]).

-include("skel.hrl").

-spec start(pos_integer(), pid(), pid()) -> 'eos'.
%% @doc Initialises the collector; forwards any and all output from the inner-
%% workflow to the sink process at `NextPid'.
start(NWorkers, EmitterPid, NextPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{num_workers, NWorkers}, {next_pid, NextPid}]),
  loop(NWorkers, EmitterPid, NextPid).

-spec loop(pos_integer(), pid(), pid()) -> 'eos'.
%% @doc Worker-function for {@link start/2}. Recursively receives, and 
%% forwards, any output messages from the inner-workflow. Halts when the `eos' 
%% system message is received, and only one active worker process remains.
loop(NWorkers, EmitterPid, NextPid) ->
  receive
    {data, {complete,WPID,Value}, Extra} ->
	  DataMessage = {data,Value,Extra},
	  sk_tracer:t(50, self(), NextPid, {?MODULE, data}, [{input, DataMessage}]),
	  NextPid ! DataMessage,
	  EmitterPid ! {complete,WPID},
	  loop(NWorkers, EmitterPid, NextPid);
    {system, eos} when NWorkers =< 1 ->
      sk_tracer:t(75, self(), NextPid, {?MODULE, system}, [{msg, eos}, {remaining, 0}]),
      NextPid ! {system, eos},
      eos;
    {system, eos} ->
      sk_tracer:t(85, self(), {?MODULE, system}, [{msg, eos}, {remaining, NWorkers-1}]),
      loop(NWorkers-1, EmitterPid, NextPid)
  end.

