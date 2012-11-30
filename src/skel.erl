%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module is the root module of the `skel` library, including types
%%% and entry-point functions.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(skel).

-export([
         run/2
        ]).

-export_type([
              workflow/0 ,wf_item/0
             ,worker_fun/0
             ,decomp_fun/0 ,recomp_fun/0 ,reduce_fun/0 ,filter_fun/0
             ,maker_fun/0
             ,input/0
             ,system_message/0
             ,data_message/0
             ,dm_identifier/0
             ,data_fun/0
             ,data_decomp_fun/0 ,data_recomp_fun/0 ,data_reduce_fun/0 ,data_filter_fun/0
             ]).

-include("skel.hrl").

-type workflow()  :: [wf_item(),...].
-type wf_item()   :: {seq,      worker_fun()}
                   | {ord,      workflow()}
                   | {farm,     workflow(), pos_integer()}
                   | {decomp,   workflow(), decomp_fun(), recomp_fun()}
                   | {map,      workflow(), decomp_fun(), recomp_fun()}
                   | {reduce,   decomp_fun(), reduce_fun()}
                   | {feedback, workflow(), filter_fun()}.

-type worker_fun()  :: fun((any())        -> any()).
-type decomp_fun()  :: fun((any())        -> [any(),...]).
-type recomp_fun()  :: fun(([any(),...])  -> any()).
-type reduce_fun()  :: fun((any(), any()) -> any()).
-type filter_fun()  :: fun((any())        -> boolean()).
-type maker_fun()   :: fun((pid())        -> pid()).

-type input()       :: list() | module().

-type system_message()    :: {system, sm_item()}.               % These are all the system-level messages. EOS, coordination etc.
-type data_message()      :: {data, any(), [dm_identifier()]}.  % The data being passed, and a stack of information as to its identity and ordering (used in ordered skeletons and maps)
-type dm_identifier()     :: feedback
                           | {ord, pos_integer()}
                           | {decomp, reference(), pos_integer(), pos_integer()}
                           | {reduce, reference(), number()}.
-type sm_item()           :: eos
                           | {reduce_unit, reference(), number()}
                           | {feedback_setup, pid(), reference()}
                           | {feedback_reply, pid(), reference()}
                           | {counter, term(), pid()}.

-type data_fun()        :: fun((data_message())                 -> data_message()).
-type data_decomp_fun() :: fun((data_message())                 -> [data_message(),...]).
-type data_recomp_fun() :: fun(([data_message(),...])           -> data_message()).
-type data_reduce_fun() :: fun((data_message(), data_message()) -> data_message()).
-type data_filter_fun() :: fun((data_message())                 -> boolean()).

-spec run(workflow(), input()) -> pid().
run(WorkFlow, Input) ->
  sk_assembler:run(WorkFlow, Input).

