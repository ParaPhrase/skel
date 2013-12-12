%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% 
%%% @doc This module contains the Map skeleton initialisation logic.
%%% 
%%% The Map skeleton is a parallel map. The skeleton applies a given function 
%%% to the elements within one or more lists.
%%% 
%%% This implementation assumes a list of lists as input, where the 
%%% decomposition of said input may be expressed as the identity function. 
%%% Whilst this implementation of Map usually determines the number of worker 
%%% processes it needs automatically, the developer may explicitly set this, 
%%% as in {@link sk_farm}.
%%% 
%%% 
%%% === Example ===
%%% 
%%% 	```skel:do([{map, [{seq, fun ?MODULE:f/1}]}], Input).'''
%%% 
%%% 	Here we use a Map skeleton to perform a function `f/1' over all 
%%% 	elements for all lists represented by `Input'. Returned, we receive a 
%%% 	list of lists the same as `Input' itself, bar that the elements of 
%%% 	each are the result of their application to `f/1'.
%%% 
%%% 	In this example we note that the number of worker processes the Map 
%%% 	skeleton uses is determined by the length of the longest list in 
%%% 	`Input'. To constrain, or otherwise set this value, we might add an 
%%% 	extra term to the Map tuple.
%%% 
%%% 	```skel:do([{map, [{seq, fun ?MODULE:f/1}], 10}], Input).'''
%%% 
%%% 	Using the same example, we now note that the number of worker 
%%% 	processes used is set to ten. Performance comparisons between these 
%%% 	two depends heavily on the chosen `Input', and the machine on which it 
%%% 	runs.
%%% 
%%% @end
%%%----------------------------------------------------------------------------

-module(sk_map).

-export([make/1, make/2]).

-include("skel.hrl").

-spec make(workflow()) -> maker_fun().
%% @doc Initialises an instance of the Map skeleton ready to receive inputs, 
%% where the number of worker processes is automatic. The function or 
%% functions to be applied to said inputs are given under `WorkFlow'.
%% 
%% A combiner, or recomposition, process is created and acts as a sink for the 
%% workers. These workers are generated and managed from within a
%% {@link sk_map_partitioner} process.
make(WorkFlow) ->
  fun(NextPid) ->
    CombinerPid = spawn(sk_map_combiner, start, [NextPid]),
    spawn(sk_map_partitioner, start, [auto, WorkFlow, CombinerPid])
  end.


-spec make(workflow(), pos_integer()) -> maker_fun().
%% @doc Initialises an instance of the Map skeleton ready to receive inputs, 
%% using a given number of worker processes. This number is specified under 
%% `NWorkers', and the function or functions to be applied to any and all 
%% inputs are given by `WorkFlow'.
%% 
%% A combiner, or recomposition, process is created, and acts as a sink for 
%% the workers. These workers are initialised with the specified workflow, and 
%% their Pids passed to a {@link sk_map_partitioner} process.
make(WorkFlow, NWorkers) ->
  fun(NextPid) ->
    CombinerPid = spawn(sk_map_combiner, start, [NextPid, NWorkers]),
    WorkerPids = sk_utils:start_workers(NWorkers, WorkFlow, CombinerPid),
    spawn(sk_map_partitioner, start, [man, WorkerPids, CombinerPid])
  end.
