%%%----------------------------------------------------------------------------
%%% author Sam Elliott <ashe@st-andrews.ac.uk>
%%% copyright 2012 University of St Andrews (See LICENCE)
%%%----------------------------------------------------------------------------
%% Defines the workflow concept. A workflow is a specification that defines how work should be undertaken, it is a list of one or more skeletons.
-type workflow()  :: [wf_item(),...]. 

-type wf_item()   :: {seq,      worker_fun()}
                   | {pipe,     workflow()}
                   | {ord,      workflow()}
                   | {farm,     workflow(), pos_integer()}
                   | {hyb_farm, workflow(), workflow(), pos_integer(), pos_integer()}
                   | {cluster,  workflow(), decomp_fun(), recomp_fun()}
                   | {map,      workflow()}
                   | {map,      workflow(), pos_integer()}
                   | {hyb_map,  workflow(), workflow(), pos_integer(), pos_integer()}
                   | {reduce,   reduce_fun(), decomp_fun()}
                   | {feedback, workflow(), filter_fun()}.
% Workflow items (skeletons) and their content. 

-type worker_fun()  :: fun((any())        -> any()). % Any function that is performed by a worker unit.

-type decomp_fun()  :: fun((any())        -> [any(),...]). % Any function that decomposes its input into several parts, that may be used by different workers.

-type recomp_fun()  :: fun(([any(),...])  -> any()). % Any function that recomposes decomposed parts.

-type reduce_fun()  :: fun((any(), any()) -> any()). % Any function that reduces two inputs to a single output.

-type filter_fun()  :: fun((any())        -> boolean()). % Any function that determines whether an input matches one or more given criteria.




-type input()       :: list() | module(). % Definition for any possible worker function input.




-type system_message()    :: {system, sm_item()}.
%% These are all the system-level messages. EOS, coordination etc.

-type data_message()      :: {data, any(), [dm_identifier()]}.
%% The data being passed, and a stack of information as to its identity and 
%% ordering (used in ordered skeletons and maps).

-type dm_identifier()     :: feedback
                           | {ord, pos_integer()}
                           | {decomp, reference(), pos_integer(), pos_integer()}
                           | {reduce, reference(), number()}.
%% Used to identify different types of data message.

-type sm_item()           :: eos
                           | {reduce_unit, reference(), number()}
                           | {feedback_setup, pid(), reference()}
                           | {feedback_reply, pid(), reference()}
                           | {counter, term(), pid()}.
%% Used to identify different types of system message.




-type data_fun()        :: fun((data_message())       -> data_message()).
%% Any function that takes a data message as input, and similarly returns a
%% data message.

-type data_decomp_fun() :: fun((data_message())       -> [data_message(),...]).
%% Any function that decomposes, or splits, a single data message. Producing a 
%% list of data messages, each containing a fragment of the input's message.

-type data_recomp_fun() :: fun(([data_message(),...]) -> data_message()).
%% Any function that recomposes, or joins, a list of data messages. Produces a 
%% single data message containing all information in the input.

-type data_reduce_fun() :: fun((data_message(), data_message()) -> data_message()).
%% Any function that reduces two data messages to a single data message.

-type data_filter_fun() :: fun((data_message())       -> boolean()).
%% Any function that determines whether a data message satisfies some given 
%% constraint.
