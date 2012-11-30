%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains the collector logic of a `farm` skeleton.
%%%
%%% A farm skeleton has the most basic kind of stream parallelism - inputs are
%%% sent to one of `n` replicas of the inner skeleton for processing.
%%%
%%% The collector takes inputs off the inner skeletons' output stream and sends
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

-ifdef(TEST).
-compile(export_all).
-endif.

-spec start(pos_integer(), pid()) -> 'eos'.
start(NWorkers, NextPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{num_workers, NWorkers}, {next_pid, NextPid}]),
  loop(NWorkers, NextPid).

-spec loop(pos_integer(), pid()) -> 'eos'.
loop(NWorkers, NextPid) ->
  receive
    {data, _, _} = DataMessage ->
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

