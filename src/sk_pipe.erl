%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2013 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% 
%%% @doc This module contains 'pipe' skeleton initialization logic.
%%% 
%%% The Pipe skeleton is the sequential composition of several skeletons. It 
%%% applies those skeletons and/or functions to a sequence of independent 
%%% inputs where the output of one function is the input of the next. 
%%% 
%%% 
%%% <tt>Example</tt>
%%% 
%%% ```skel:run([{pipe, [{seq, fun ?MODULE:f1/1}, {seq, fun ?MODULE:f2/1}, 
%%% 	{seq, fun ?MODULE:f3/1}]}], Inputs)'''
%%% 
%%% 	In this example, the pipeline has three stages. Each stage uses a 
%%% 	sequential function to perform its task -- i.e. executing the 
%%% 	developer-defined `f1', `f2', and `f3' respectively. The input, 
%%% 	represented as the list `Inputs', is first passed to `f1' whose 
%%% 	results form the input for `f2', the results of which serve as input 
%%% 	to `f3'.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_pipe).

-export([
         start/2
        ]).

-include("skel.hrl").


%% @doc Produces workers according to the specified workflow.
-spec start( {workflow()}, pid()) ->
               pid().
start({WorkFlow}, NextPid) ->
  sk_assembler:make(WorkFlow, NextPid).

