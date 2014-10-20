%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% 
%%% @doc This module contains the intitialisation logic of a Cluster wrapper. 
%%%
%%% The cluster wrapper acts in a similar manner to the Map skeleton, but 
%%% allows the developer to customise the decomposition and recomposition 
%%% functions used. Inputs are decomposed according to the developer-defined 
%%% decomposition function, and then passed to the worker process running the 
%%% inner-workflow.
%%%
%%% Additionally, the cluster wrapper allows the developer to determine the 
%%% level of clustering of inputs. Hence, the identifying atom `cluster'.
%%% 
%%% 
%%% === Example ===
%%% 
%%% 	```skel:do([{cluster, [{farm, [{seq, fun ?MODULE:f/1}], 10}], fun ?MODULE:decomp/1, fun ?MODULE:recomp/1}], Input).'''
%%% 
%%% 	We are able to replicate the second Map example using the above 
%%% 	cluster workflow item. Where `decomp/1' and `recomp/1' are developer-
%%% 	defined, and `f/1' remains the function to be mapped to each element 
%%% 	in every input. Here we use an inner task farm with the ten workers to 
%%% 	repeatedly apply `f/1' to each partite element, but a different 
%%% 	arrangement of nested skeletons may also be used.
%%%
%%% @end
%%%----------------------------------------------------------------------------


-module(sk_cluster).

-export([
         start/2
        ]).

-include("skel.hrl").




%% @doc Initialises the Cluster wrapper using both the developer-defined 
%% functions under `Decomp' and `Recomp' as decomposition and recomposition 
%% functions respectively. 
%% 
%% Inputs are decomposed, sent through the specified (inner) workflow, and 
%% then recomposed to be delivered as output.
-spec start({workflow(), decomp_fun(), recomp_fun()}, pid()) ->
               pid().
start({WorkFlow, Decomp, Recomp}, NextPid) ->
    RecompPid = proc_lib:spawn(sk_cluster_recomp, start, [Recomp, NextPid]),
    WorkerPid = sk_utils:start_worker(WorkFlow, RecompPid),
    proc_lib:spawn(sk_cluster_decomp, start, [Decomp, WorkerPid]).

