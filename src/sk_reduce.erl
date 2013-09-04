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
         make/1
        ,fold1/2
        ]).


-ifdef(TEST).
-compile(export_all).
-endif.

-spec make( list() ) -> skel:maker_fun().
make(Proplist) ->
  make ( _Reduce = proplists:get_value( reduce, Proplist),
         _Decomp = proplists:get_value( decomp, Proplist)).

-spec make(skel:decomp_fun(), skel:reduce_fun()) -> skel:maker_fun().
make(Reduce, Decomp) when is_function(Reduce, 2),
                          is_function(Decomp, 1) ->
  fun(NextPid) ->
    % We don't start workers until we know how many partitions we have.
    spawn(sk_reduce_decomp, start, [Decomp, Reduce, NextPid])
  end.

% Implemented as a treefold underneath
-spec fold1(fun((A, A) -> A), [A,...]) -> A when
    A :: term().
fold1(_ReduceFun, [L1]) ->
  L1;
fold1(ReduceFun, [L1, L2 | List]) ->
  fold1(ReduceFun, [ReduceFun(L1, L2) | pairs(ReduceFun, List)]).

-spec pairs(fun((A, A) -> A), [A]) -> [A] when
    A :: term().
pairs(Fun, [L1,L2|List]) ->
  [Fun(L1,L2) | pairs(Fun, List)];
pairs(_Fun, List) ->
  List.
