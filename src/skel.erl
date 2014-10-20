%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% @doc This module is the root module of the 'Skel' library, including 
%%% entry-point functions.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(skel).

-export([
         run/2
        ,do/2
        ]).

-include("skel.hrl").


%% @doc Primary entry-point function to the Skel library. Runs a specified 
%% workflow passing <tt>Input</tt> as input. Does not receive or return any
%% output from the workflow.
%% 
%% <h5>Example:</h5>
%%    ```skel:run([{seq, fun ?MODULE:p/1}], Images)'''
%%
%%    Here, skel runs the function <tt>p</tt> on all items in the 
%%    list <tt>Images</tt> using the Sequential Function wrapper.
%%
-spec run(workflow(), input()) ->
             pid().
run(WorkFlow, Input) ->
  sk_assembler:run(WorkFlow, Input).


%% @doc The second entry-point function to the Skel library. This function 
%% <em>does</em> receive and return the results of the given workflow.
%% 
%% <h5>Example:</h5>
%%    ```skel:do([{reduce, fun ?MODULE:reduce/2, fun ?MODULE:id/1}], Inputs)]'''
%%
%%      In this example, Skel uses the Reduce skeleton, where <tt>reduce</tt> 
%%      and <tt>id</tt> are given as the reduction and decomposition functions 
%%      respectively. The result for which is returned, and so can be printed 
%%      or otherwise used.
%%
-spec do(workflow(), list()) ->
            list().
do(WorkFlow, Input) ->
  run(WorkFlow, Input),
  receive
    {sink_results, Results} -> 
        Results
  end.

