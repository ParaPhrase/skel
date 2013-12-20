%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%%
%%% @doc This module takes a workflow specification, and converts it in into a
%%% set of (concurrent) running processes.
%%%
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_assembler).

-export([
         make/2
        ,run/2
        ]).

-include("skel.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-spec make(workflow(), pid() | module()) -> pid() .
%% @doc Function to produce a set of processes according to the given workflow 
%% specification.
make(WorkFlow, EndModule) when is_atom(EndModule) ->
  DrainPid = sk_sink:start_mod(EndModule),
  make(WorkFlow, DrainPid);
make(WorkFlow, EndPid) when is_pid(EndPid) ->
  lists:foldr(fun start_item/2,
              EndPid,
              WorkFlow).

-spec run( workflow(), input()) -> pid().
%% @doc Function to produce and start a set of processes according to the 
%% given workflow specification and input.
run(WorkFlow, Input) when is_list(WorkFlow) ->
  DrainPid = sk_sink:start_acc(),
  AssembledWF = make(WorkFlow, DrainPid),
  sk_source:start(Input, AssembledWF ).



start_item( Fun, NextPid ) when is_function(Fun) ->
  start_item( { seq, Fun }, NextPid );
start_item( Item, NextPid ) ->
  { ItemName, Parameters } = split_item( Item ),
  ModuleName = item_to_module( ItemName ),
  ModuleName:start( Parameters, NextPid ).


split_item( Item ) ->
  { erlang:element( 1, Item ),
    erlang:delete_element( 1, Item)}.


item_to_module( seq )      -> sk_seq;
item_to_module( pipe )     -> sk_pipe;
item_to_module( ord )      -> sk_ord;
item_to_module( farm )     -> sk_farm;
item_to_module( cluster )  -> sk_cluster;
item_to_module( map )      -> sk_map;
item_to_module( reduce )   -> sk_reduce;
item_to_module( feedback ) -> sk_feedback.

