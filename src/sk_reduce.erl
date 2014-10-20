%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% 
%%% @doc This module contains Reduce skeleton initialization logic.
%%%
%%% A reduce skeleton is an implementation of a parallel treefold.
%%% 
%%% === Example ===
%%% 
%%%   ```skell:run([{reduce, fun?MODULE:reduce/2, fun ?MODULE:id/1}], Inputs)'''
%%% 
%%%   Here, we call upon the reduce skeleton to reduce a list of inputs, 
%%%   denoted `Inputs', using the developer-defined functions `reduce' and `id'. In this example, we presume to sum the elements in a list. Hence, `reduce' takes two arguments and returns their total. Whilst, `id' returns its input sans transformation. We receive the answer in the form of a single-element list as a message from the sink process.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_reduce).

-export([
         start/2
        ]).

-include("skel.hrl").


%% @doc Readies an instance of the Reduce skeleton. Uses the developer-defined 
%% decomposition and recomposition functions `Decomp' and `Reduce', 
%% respectively. Returns an anonymous function waiting for the sink process 
%% `NextPid'.
-spec start( Parameters, NextPid ) -> WorkflowPid when
    Parameters :: { Reduce :: reduce_fun(),
                    Decomp :: decomp_fun() },
    NextPid :: pid(),
    WorkflowPid :: pid().

start({Reduce, Decomp}, NextPid ) ->
  io:format("start reduce~n~n"),
  proc_lib:spawn(sk_reduce_decomp, start, [Decomp, Reduce, NextPid]).


