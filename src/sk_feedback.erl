%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% 
%%% @doc This module contains 'feedback' skeleton initialization logic.
%%%
%%% The Feedback skeleton repeatedly passes output from its inner-workflow 
%%% back into said workflow until a given constraint-checking function fails.
%%%
%%% === Example ===
%%% 
%%% 	```skel:do([{feedback, [{seq, fun ?MODULE:succ/1}], fun ?MODULE:less_than/1}], Input).'''
%%% 
%%% 	Here we use the feedback skeleton to ensure all elements of the list 
%%% 	Input are above a certain value. If any one element is equal to or 
%%% 	greater than that value, those elements will be recursively brought up 
%%% 	to that value. In the above example, the method `succ/1' adds 
%%% 	one to its input, where `less_than/1' acts as a check.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_feedback).

-export([
         make/2
        ]).

-include("skel.hrl").

-spec make(workflow(), filter_fun()) -> maker_fun().
%% @doc Initialises a Feedback skeleton.
%% 
%% Constraint-checking filter processes are produced using the developer-
%% defined function `FilterFun', and are used to check inputs according to 
%% said function.
make(WorkFlow, FilterFun) ->
  fun(NextPid) ->
    Ref = make_ref(),
    CounterPid = spawn(sk_feedback_bicounter, start, []),
    FilterPid = spawn(sk_feedback_filter, start, [FilterFun, Ref, CounterPid, NextPid]),
    WorkerPid = sk_utils:start_worker(WorkFlow, FilterPid), 
    spawn(sk_feedback_receiver, start, [Ref, CounterPid, FilterPid, WorkerPid])
  end.
