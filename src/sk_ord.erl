%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% 
%%% @doc This module contains 'ord' skeleton initialization logic.
%%%
%%% The 'ord' skeleton can reorder outputs from its inner skeletons such that
%%% they have the same order coming out the ord skeleton as they had going into
%%% it.
%%%
%%% This becomes useful when requiring ordering on things like a farm.
%%% 
%%% === Example ===
%%% 
%%%   ```skel:run([{ord, [{farm, [{seq, fun ?MODULE:p/1}], 10}]}], Inputs)'''
%%%
%%%   In this example we wrap the `ord' skeleton around a farm of ten 
%%%   workers, each of which are running the developer-defined `p/1' in 
%%%   sequential functions. The result of this is that the returned 
%%%   applications of the input to `p/1' is in the same order as the list 
%%%   of inputs `Inputs' itself.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_ord).

-export([
         start/2
        ]).

-include("skel.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-spec start(workflow() , pid() ) -> maker_fun().
%% @doc Constructs an Ord skeleton wrapper to ensure the outputs of the 
%% specified workflow are in the same order as that of its inputs.
start({WorkFlow}, NextPid ) ->
  ReordererPid = spawn(sk_ord_reorderer, start, [NextPid]),
  WorkerPid = sk_utils:start_worker(WorkFlow, ReordererPid),
  spawn(sk_ord_tagger, start, [WorkerPid]).

