%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains `map` skeleton initialization logic.
%%%
%%% The `map` skeleton, takes each input, decomposes it, and puts each of the 
%%% decomposed parts through their own inner skeletons. After they have gone 
%%% through the inner skeletons, they are recomposed into a single input before
%%% being forwarded to the next skeleton.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_map).

-export([
         make/1
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-spec make(list()) -> skel:maker_fun().
make(Proplist) ->
  make ( _Workflow = proplists:get_value( do, Proplist),
         _Decomp = proplists:get_value( decomp, Proplist),
         _Recomp = proplists:get_value( recomp, Proplist)).


-spec make(skel:workflow(), skel:decomp_fun(), skel:recomp_fun()) -> fun((pid()) -> pid()).
make(WorkFlow, Decomp, Recomp) when is_function( Decomp, 1 ),
                                    is_function( Recomp, 1)->
  fun(NextPid) ->
    CombinerPid = spawn(sk_map_combiner, start, [Recomp, NextPid]),
    % We don't start workers until we know how many partitions we need.
    spawn(sk_map_partitioner, start, [Decomp, WorkFlow, CombinerPid])
  end.
