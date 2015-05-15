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
         make/3,
	 make_hyb/4,
	 make_hyb/5,
	 make_hyb/7
        ]).

-include("skel.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-spec make(workflow(), decomp_fun(), recomp_fun()) -> fun((pid()) -> pid()).
%% @doc Initialises the Cluster wrapper using both the developer-defined 
%% functions under `Decomp' and `Recomp' as decomposition and recomposition 
%% functions respectively. 
%% 
%% Inputs are decomposed, sent through the specified (inner) workflow, and 
%% then recomposed to be delivered as output.
make(WorkFlow, Decomp, Recomp) ->
  fun(NextPid) ->
    RecompPid = spawn(sk_cluster_recomp, start, [Recomp, NextPid]),
    WorkerPid = sk_utils:start_worker(WorkFlow, RecompPid),
    spawn(sk_cluster_decomp, start, [Decomp, WorkerPid])
  end.

ceiling(X) ->
    T = erlang:trunc(X),
    case (X - T) of
        Neg when Neg < 0 -> T;
        Pos when Pos > 0 -> T + 1;
        _ -> T
    end.


mark_tasks([], _NCPUWorkers, _NGPUWorkers) ->
    [];
mark_tasks(_Tasks, 0, 0) ->
    [];
mark_tasks([Task|Tasks], 0, NGPUWorkers) ->
    [{gpu, Task} | mark_tasks(Tasks, 0, NGPUWorkers-1)];
mark_tasks([Task|Tasks], NCPUWorkers, NGPUWorkers) ->
    [{cpu, Task} | mark_tasks(Tasks, NCPUWorkers-1, NGPUWorkers)].

hyb_cluster_decomp(Decomp, NCPUWorkers, NGPUWorkers, Input) ->
    Tasks = Decomp(Input),
    mark_tasks(Tasks, NCPUWorkers, NGPUWorkers).

calculate_ratio(TimeRatio, NTasks, NCPUW, NGPUW) ->
    TasksCPU = lists:seq(0, NTasks),
    Time = fun(CPUTasks, GPUTasks) ->
		   max (ceiling(CPUTasks/NCPUW)*TimeRatio, ceiling(GPUTasks/NGPUW))
	   end,
    Ratio = lists:foldl(fun(Elem,Acc) -> FooBar = Time(Elem, NTasks-Elem),
					 if
					    (FooBar < element(1,Acc)) or (element(1,Acc) == -1) 
					     -> {FooBar, Elem};
					    true -> Acc
					end end,
			{-1,0}, TasksCPU),
    {element(2,Ratio), NTasks-element(2,Ratio)}.

calculate_chunk_sizes(NrItems, NrWorkers) ->
    ChunkSize = NrItems div NrWorkers,
    Remainder = NrItems rem NrWorkers,
    ChunkSizes = lists:duplicate(Remainder, {ChunkSize+1}) ++ lists:duplicate(NrWorkers-Remainder, {ChunkSize}),
    ChunkSizes.

create_task_list([], [], _MakeChunkFun, _Input) ->
    [];
create_task_list([CPUChunk|CPUChunks], GPUChunks, MakeChunkFun, Input) ->
    CPUChunkSize = element(1,CPUChunk),
    {Work, Rest} = MakeChunkFun(Input, CPUChunkSize),
    [ {cpu, Work} | create_task_list(CPUChunks, GPUChunks, MakeChunkFun, Rest) ];
create_task_list([], [GPUChunk|GPUChunks], MakeChunkFun, Input) ->
    GPUChunkSize = element(1,GPUChunk),
    {Work, Rest} = MakeChunkFun(Input, GPUChunkSize),
    [ {gpu, Work} | create_task_list([], GPUChunks, MakeChunkFun, Rest) ].

hyb_cluster_decomp_default(TimeRatio, StructSizeFun, MakeChunkFun, NCPUWorkers, NGPUWorkers, Input) ->
    NItems = StructSizeFun(Input),
    {CPUItems, GPUItems} = if
		(NCPUWorkers>0) and (NGPUWorkers>0) -> calculate_ratio(TimeRatio, NItems, NCPUWorkers, NGPUWorkers);
		NGPUWorkers == 0 -> {NItems,0};
		NCPUWorkers == 0 -> {0, NItems}
	    end,
    CPUChunkSizes = calculate_chunk_sizes(CPUItems, NCPUWorkers),
    GPUChunkSizes = calculate_chunk_sizes(GPUItems, NGPUWorkers),
    [create_task_list(CPUChunkSizes, GPUChunkSizes, MakeChunkFun, Input)].

-spec make_hyb(workflow(), decomp_fun(), recomp_fun(), pos_integer(), pos_integer()) -> fun((pid()) -> pid()).
make_hyb(Workflow, Decomp, Recomp, NCPUWorkers, NGPUWorkers) ->
    fun(NextPid) ->
	    RecompPid = spawn(sk_cluster_recomp, start, [Recomp, NextPid]),
	    WorkerPid = sk_utils:start_worker_hyb(Workflow, RecompPid, NCPUWorkers, NGPUWorkers),
	    spawn(sk_cluster_decomp, start, [fun (Input) -> hyb_cluster_decomp(Decomp, NCPUWorkers, NGPUWorkers, Input) end,
					    WorkerPid])
    end.

-spec make_hyb(workflow(), float(), fun((any()) -> pos_integer()), fun((any(),pos_integer()) -> pos_integer()),
	       fun((any())->any()),
	       pos_integer(), pos_integer()) -> fun((pid()) -> pid()).
make_hyb(Workflow, TimeRatio, StructSizeFun, MakeChunkFun, RecompFun, NCPUWorkers, NGPUWorkers) ->
    fun(NextPid) ->
	    RecompPid = spawn(sk_cluster_recomp, start, [RecompFun, NextPid]),
	    WorkerPid = sk_utils:start_worker_hyb(Workflow, RecompPid, NCPUWorkers, NGPUWorkers),
	    spawn(sk_cluster_decomp, start, [fun (Input) -> hyb_cluster_decomp_default(TimeRatio, StructSizeFun, MakeChunkFun, NCPUWorkers, NGPUWorkers, Input) end,
					    WorkerPid])
    end.
    
-spec make_hyb(workflow(), float(), pos_integer(), pos_integer()) -> fun((pid())->pid()).
make_hyb(Workflow, TimeRatio, NCPUWorkers, NGPUWorkers) ->
    fun(NextPid) ->
	    RecompPid = spawn(sk_cluster_recomp, start, [fun lists:flatten/1, NextPid]),
	    WorkerPid = sk_utils:start_worker_hyb(Workflow, RecompPid, NCPUWorkers, NGPUWorkers),
	    spawn(sk_cluster_decomp, start, [fun (Input) -> hyb_cluster_decomp_default(TimeRatio, fun length/1,fun (Data,Pos) -> lists:split(Pos,Data) end, NCPUWorkers, NGPUWorkers, Input) end,
					    WorkerPid])
    end.
    
