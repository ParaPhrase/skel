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
  MakeFns = [parse(Section) || Section <- WorkFlow],
  lists:foldr(fun(MakeFn, Pid) -> MakeFn(Pid) end, EndPid, MakeFns).

-spec run(pid() | workflow(), input()) -> pid().
%% @doc Function to produce and start a set of processes according to the 
%% given workflow specification and input.
run(WorkFlowPid, Input) when is_pid(WorkFlowPid) ->
  Feeder = sk_source:make(Input),
  Feeder(WorkFlowPid);
run(WorkFlow, Input) when is_list(WorkFlow) ->
  DrainPid = (sk_sink:make())(self()),
  AssembledWF = make(WorkFlow, DrainPid),
  run(AssembledWF, Input).

-spec parse(wf_item()) -> maker_fun().
%% @doc Determines the course of action to be taken according to the type of 
%% workflow specified. Constructs and starts specific skeleton instances.
parse(Fun) when is_function(Fun, 1) ->
  parse({seq, Fun});
parse({seq, Fun}) when is_function(Fun, 1) ->
  sk_seq:make(Fun);
parse({pipe, WorkFlow}) ->
  sk_pipe:make(WorkFlow);
parse({ord, WorkFlow}) ->
  sk_ord:make(WorkFlow);
parse({farm, WorkFlow, NWorkers}) ->
  sk_farm:make(NWorkers, WorkFlow);
parse({map, WorkFlow}) ->
  sk_map:make(WorkFlow);
parse({map, WorkFlow, NWorkers}) ->
  sk_map:make(WorkFlow, NWorkers);
parse({cluster, WorkFlow, Decomp, Recomp}) when is_function(Decomp, 1),
                                               is_function(Recomp, 1) ->
  sk_cluster:make(WorkFlow, Decomp, Recomp);
parse({reduce, Reduce, Decomp}) when is_function(Reduce, 2),
                                     is_function(Decomp, 1) ->
  sk_reduce:make(Decomp, Reduce);
parse({feedback, WorkFlow, Filter}) when is_function(Filter, 1) ->
  sk_feedback:make(WorkFlow, Filter).


