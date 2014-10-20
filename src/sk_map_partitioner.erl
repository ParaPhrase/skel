%%%----------------------------------------------------------------------------
%%% @author Sam Elliot <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% 
%%% @doc This module contains the simple 'map' skeleton partitioner logic.
%%% 
%%% The Map skeleton is a parallel map. The skeleton applies a given function 
%%% to the elements within one or more lists.
%%% 
%%% The partitioner takes the input list, dispatching each partite element to 
%%% a pool of worker processes. These worker processes apply the 
%%% developer-defined function to each partite element. 
%%% 
%%% This module supports both the automatic creation of worker processes, and 
%%% the ability to define the exact number to be used. With the former, the 
%%% minimal number of workers are created for all inputs. This is given by the 
%%% number of elements in the longest list.
%%% 
%%% @end
%%%----------------------------------------------------------------------------

-module(sk_map_partitioner).

-export([
         start/3,
         start/4,
         start/5
        ]).

%% Privete exports
-export([
         loop/4,
         loop/3
        ]).

-include("skel.hrl").


-spec start(atom(), workflow() | [pid()], pid()) -> 'eos'.
%% @doc Starts the recursive partitioning of inputs. 
%% 
%% If the number of workers to be used is specified, a list of Pids for those 
%% worker processes are received through `WorkerPids' and the second clause 
%% used. Alternatively, a workflow is received as `Workflow' and the first 
%% clause is used. In the case of the former, the workers have already 
%% been initialised with their workflows and so the inclusion of the 
%% `WorkFlow' argument is unneeded in this clause. 
%% 
%% The atoms `auto' and `main' are used to determine whether a list of worker 
%% Pids or a workflow is received. `CombinerPid' specifies the Pid of the 
%% process that will recompose the partite elements following their 
%% application to the Workflow.
%% 
%% @todo Wait, can't this atom be gotten rid of? The types are sufficiently different.
start(auto, WorkFlow, CombinerPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{combiner, CombinerPid}]),
  proc_lib:spawn( ?MODULE, loop, [decomp_by(), WorkFlow, CombinerPid, []]).

start(man, WorkFlow, NWorkers, CombinerPid) ->
  WorkerPids = sk_utils:start_workers(NWorkers, WorkFlow, CombinerPid),
  sk_tracer:t(75, self(), {?MODULE, start}, [{combiner, CombinerPid}]),
  proc_lib:spawn(?MODULE, loop, [decomp_by(), CombinerPid, WorkerPids]).

-spec start( pos_integer(), pos_integer(), workflow(), workflow()) -> pid().
start(NCPUWorkers, NGPUWorkers, WorkFlowCPU, WorkFlowGPU, CombinerPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{combiner, CombinerPid}]),
  WorkerPids = sk_utils:start_workers_hyb(NCPUWorkers, NGPUWorkers, WorkFlowCPU, WorkFlowGPU, CombinerPid),
  proc_lib:spawn(?MODULE, loop, [man, WorkerPids, CombinerPid]).

-spec loop(data_decomp_fun(), workflow(), pid(), [pid()]) -> 'eos'.
%% @private
%% @doc Recursively receives inputs as messages, which are decomposed, and the 
%% resulting messages sent to individual workers. `loop/4' is used in place of 
%% {@link loop/3} when the number of workers to be used is automatically 
%% determined by the total number of partite elements of an input.
loop(DataPartitionerFun, WorkFlow, CombinerPid, WorkerPids) ->
  receive
    {data, _, _} = DataMessage ->
      PartitionMessages = DataPartitionerFun(DataMessage),
      WorkerPids1 = start_workers(length(PartitionMessages), WorkFlow, CombinerPid, WorkerPids),
      Ref = make_ref(), %% FIXME not sure what for this ref is
      sk_tracer:t(60, self(), {?MODULE, data}, [{ref, Ref}, {input, DataMessage}, {partitions, PartitionMessages}]),
      dispatch(Ref, length(PartitionMessages), PartitionMessages, WorkerPids1),
      loop(DataPartitionerFun, WorkFlow, CombinerPid, WorkerPids1);
    {system, eos} ->
      sk_utils:stop_workers(?MODULE, WorkerPids),
      eos
  end.


-spec loop(data_decomp_fun(), pid(), [pid()]) -> 'eos'.
%% @doc Recursively receives inputs as messages, which are decomposed, and the 
%% resulting messages sent to individual workers. `loop/3' is used in place of 
%% {@link loop/4} when the number of workers is set by the developer.
loop(DataPartitionerFun, CombinerPid, WorkerPids) ->
  receive
    {data, _, _} = DataMessage ->
      PartitionMessages = DataPartitionerFun(DataMessage),
      Ref = make_ref(),
      sk_tracer:t(60, self(), {?MODULE, data}, [{ref, Ref}, {input, DataMessage}, {partitions, PartitionMessages}]),
      dispatch(Ref, length(PartitionMessages), PartitionMessages, WorkerPids),
      loop(DataPartitionerFun, CombinerPid, WorkerPids);
    {system, eos} ->
      sk_utils:stop_workers(?MODULE, WorkerPids),
      eos
    end.


-spec decomp_by() -> data_decomp_fun().
%% @doc Provides the decomposition function and means to split a single input 
%% into many. This is based on the identity function, as the Map skeleton is 
%% applied to lists.
decomp_by() ->
  fun({data, Value, Ids}) ->
    [{data, X, Ids} || X <- Value]
  end.

-spec start_workers(pos_integer(), workflow(), pid(), [pid()]) -> [pid()].
%% @doc Used when the number of workers is not set by the developer.
%% 
%% Workers are started if the number needed exceeds the number we already 
%% have. The total number of workers is derived from the number of partitions 
%% to which `WorkFlow' will be applied, as given by `NPartitions'. This 
%% includes 'recycled' workers from previous inputs. Both new and old worker 
%% processes are returned so that they might be used. Worker processes are 
%% represented as a list of their Pids under `WorkerPids'.
start_workers(NPartitions, WorkFlow, CombinerPid, WorkerPids) when NPartitions > length(WorkerPids) ->
  NNewWorkers = NPartitions - length(WorkerPids),
  NewWorkerPids = sk_utils:start_workers(NNewWorkers, WorkFlow, CombinerPid),
  NewWorkerPids ++ WorkerPids;
start_workers(_NPartitions, _WorkFlow, _CombinerPid, WorkerPids) ->
  WorkerPids.


-spec dispatch(reference(), pos_integer(), [data_message(),...], [pid()]) -> 'ok'.
%% @doc Partite elements of input stored in `PartitionMessages' are formatted 
%% and sent to a worker from `WorkerPids'. The reference argument `Ref' 
%% ensures that partite elements from different inputs are not incorrectly 
%% included.
dispatch(Ref, NPartitions, PartitionMessages, WorkerPids) ->
  dispatch(Ref, NPartitions, 1, PartitionMessages, WorkerPids).


-spec dispatch(reference(), pos_integer(), pos_integer(), [data_message(),...], [pid()]) -> 'ok'.
%% @doc Inner-function for {@link dispatch/4}. Recursively sends each message 
%% to a worker, following the addition of references to allow identification 
%% and recomposition.
dispatch(_Ref,_NPartitions, _Idx, [], _) ->
  ok;
dispatch(Ref, NPartitions, Idx, [PartitionMessage|PartitionMessages], [WorkerPid|WorkerPids]) ->
  PartitionMessage1 = sk_data:push({decomp, Ref, Idx, NPartitions}, PartitionMessage),
  sk_tracer:t(50, self(), WorkerPid, {?MODULE, data}, [{partition, PartitionMessage1}]),
  WorkerPid ! PartitionMessage1,
  dispatch(Ref, NPartitions, Idx+1, PartitionMessages, WorkerPids ++ [WorkerPid]).
