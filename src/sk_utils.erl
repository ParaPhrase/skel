%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% @doc This module contains functions designed to start and stop worker 
%%% processes, otherwise known and referred to as simply <em>workers</em>.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_utils).

-export([
         start_workers/3
        ,start_worker/2
        ,stop_workers/2
        ]).

-include("skel.hrl").

-spec start_workers(pos_integer(), workflow(), pid()) -> [pid()].
%% @doc Starts a given number <tt>NWorkers</tt> of workers as children to the specified process <tt>NextPid</tt>. Returns a list of worker Pids.
start_workers(NWorkers, WorkFlow, NextPid) ->
  start_workers(NWorkers, WorkFlow, NextPid, []).

-spec start_workers(pos_integer(), workflow(), pid(), [pid()]) -> [pid()].
%% @doc Starts a given number <tt>NWorkers</tt> of workers as children to the 
%% specified process <tt>NextPid</tt>. Returns a list of worker Pids. Inner 
%% function to {@link start_workers/3}, providing storage for partial results.
start_workers(NWorkers,_WorkFlow,_NextPid, WorkerPids) when NWorkers < 1 ->
  WorkerPids;
start_workers(NWorkers, WorkFlow, NextPid, WorkerPids) ->
  NewWorker = start_worker(WorkFlow, NextPid),
  start_workers(NWorkers-1, WorkFlow, NextPid, [NewWorker|WorkerPids]).

-spec start_worker(workflow(), pid()) -> pid().
%% @doc Provides a worker with its tasks, the workflow <tt>WorkFlow</tt>. 
%% <tt>NextPid</tt> provides the output process to which the worker's results 
%% are sent.
start_worker(WorkFlow, NextPid) ->
  sk_assembler:make(WorkFlow, NextPid).

-spec stop_workers(module(), [pid()]) -> 'eos'.
%% @doc Sends the halt command to each worker in the given list of worker 
%% processes.
stop_workers(_Mod, []) ->
  eos;
stop_workers(Mod, [Worker|Rest]) ->
  sk_tracer:t(85, self(), Worker, {Mod, system}, [{msg, eos}]),
  Worker ! {system, eos},
  stop_workers(Mod, Rest).
