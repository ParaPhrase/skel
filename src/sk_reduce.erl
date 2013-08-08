%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains `reduce` skeleton initialization logic.
%%%
%%% A reduce skeleton is an implementation of a parallel treefold.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_reduce).

-export([
         make/2
        ,fold1/2
        ]).


-ifdef(TEST).
-compile(export_all).
-endif.

-spec make(skel:decomp_fun(), skel:reduce_fun()) -> fun((pid()) -> pid()).
make(Decomp, Reduce) ->
  fun(NextPid) ->
    % We don't start workers until we know how many partitions we have.
    spawn(sk_reduce_decomp, start, [Decomp, Reduce, NextPid])
  end.

% Implemented as a treefold underneath
-spec fold1(fun((A, A) -> A), [A,...]) -> A when A :: term().
fold1(_ReduceFun, [L1]) ->
  L1;
fold1(ReduceFun, [L1, L2 | List]) ->
  fold1(ReduceFun, [ReduceFun(L1, L2) | pairs(ReduceFun, List)]).

-spec pairs(fun((A, A) -> A), [A]) -> [A] when A :: term().
pairs(Fun, [L1,L2|List]) ->
  [Fun(L1,L2) | pairs(Fun, List)];
pairs(_Fun, List) ->
  List.
