%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% 
%%% @doc This module contains the most logic of the most basic kind of skeleton
%%% - `seq'.
%%%
%%% A 'seq' instance is a wrapper for a sequential function, taking an input
%%% from its input stream, applying the function to it and sending the result
%%% on its output stream.
%%% 
%%% === Example ===
%%% 
%%%   ```skel:run([{seq, fun ?MODULE:p/1}, {seq, fun ?MODULE:f/1}], Input).'''
%%% 
%%%     In this example, Skel is run using two sequential functions. On one 
%%%     process it runs the developer-defined `p/1' on the input `Input', 
%%%     sending all returned results to a second process. On this second 
%%%     process, the similarly developer-defined `f/1' is run on the passed 
%%%     results. This will only start once `p/1' has finished processing all 
%%%     inputs and the system message `eos' sent. Results from `f/1' are sent 
%%%     to the sink once they are available.
%%%
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_seq).

-export([
         start/2
        ,make/1
        ]).

-include("skel.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-spec make(worker_fun())  -> skel:maker_fun().
%% @doc Spawns a worker process performing the function `WorkerFun'. 
%% Returns an anonymous function that takes the parent process `NextPid'
%% as an argument. 
make(WorkerFun) ->
  fun(NextPid) ->
    spawn(?MODULE, start, [WorkerFun, NextPid])
  end.

-spec start(worker_fun(), pid()) -> eos.
%% @doc Starts the worker process' task. Recursively receives the worker 
%% function's input, and applies it to said function.
start(WorkerFun, NextPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{next_pid, NextPid}]),
  DataFun = sk_data:fmap(WorkerFun),
  loop(DataFun, NextPid).

-spec loop(skel:data_fun(), pid()) -> eos.
%% @doc Recursively receives and applies the input to the function `DataFun'. 
%% Sends the resulting data message to the process `NextPid'.
loop(DataFun, NextPid) ->
  receive
    {data,_,_} = DataMessage ->
      DataMessage1 = DataFun(DataMessage),
      sk_tracer:t(50, self(), NextPid, {?MODULE, data}, [{input, DataMessage}, {output, DataMessage1}]),
      NextPid ! DataMessage1,
      loop(DataFun, NextPid);
    {system, eos} ->
      sk_tracer:t(75, self(), NextPid, {?MODULE, system}, [{message, eos}]),
      NextPid ! {system, eos},
      eos
  end.

