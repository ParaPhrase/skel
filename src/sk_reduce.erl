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
         make/2
        ,fold1/2
        ]).

-include("skel.hrl").

-spec make(decomp_fun(), reduce_fun()) -> fun((pid()) -> pid()).
%% @doc Readies an instance of the Reduce skeleton. Uses the developer-defined 
%% decomposition and recomposition functions `Decomp' and `Reduce', 
%% respectively. Returns an anonymous function waiting for the sink process 
%% `NextPid'.
make(Decomp, Reduce) ->
  fun(NextPid) ->
    spawn(sk_reduce_decomp, start, [Decomp, Reduce, NextPid])
  end.

% Implemented as a treefold underneath

-spec fold1(fun((A, A) -> A), [A,...]) -> A when A :: term().
%% @doc Sequential `reduce' entry-point. Primarily for comparison purposes.
fold1(_ReduceFun, [L1]) ->
  L1;
fold1(ReduceFun, [L1, L2 | List]) ->
  fold1(ReduceFun, [ReduceFun(L1, L2) | pairs(ReduceFun, List)]).

-spec pairs(fun((A, A) -> A), [A]) -> [A] when A :: term().
%% @doc Second stage to {@link fold1}'s  sequential `reduce'. Recursively 
%% pairs the first two elements in the list and applies the given function 
%% `Fun'.
pairs(Fun, [L1,L2|List]) ->
  [Fun(L1,L2) | pairs(Fun, List)];
pairs(_Fun, List) ->
  List.
