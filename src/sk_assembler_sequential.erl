%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This takes a workflow specification, parses it and converts it into a
%%% function that runs a given skeleton entirely sequentially.
%%%
%%% This is primarily used so we can use the same workflow specification when
%%% we need sequential benchmarks.
%%%
%%% @headerfile "skel.hrl"
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_assembler_sequential).

-export([
         run/2
        ,compose/2
        ,id/1
        ]).

-include("skel.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-spec run(workflow(), list()) -> pid().
run(WorkFlow, Inputs) ->
  Fun = make(WorkFlow),
  self() ! {sink_results, [Fun(Input) || Input <- Inputs]},
  self().

-spec make(workflow()) -> worker_fun().
make(WorkFlow) ->
  Funs = [parse(Section) || Section <- WorkFlow],
  lists:foldl(fun ?MODULE:compose/2, fun ?MODULE:id/1, Funs).

-spec compose(fun((B) -> C), fun((A) -> B)) -> fun((A) -> C)
  when A  :: term(), B  :: term(), C  :: term().
compose(BC, AB) -> fun(X) -> BC(AB(X)) end.
-spec id(A) -> A
  when A :: term().
id(X) -> X.

-spec parse(wf_item()) -> fun().
parse(Fun) when is_function(Fun, 1) ->
  parse({seq, Fun});
parse({seq, Fun}) when is_function(Fun, 1) ->
  Fun;
parse({pipe, WorkFlow}) ->
  make(WorkFlow);
parse({ord, WorkFlow}) ->
  make(WorkFlow);
parse({farm, WorkFlow, _}) ->
  make(WorkFlow);
parse({decomp, WorkFlow, Decomp, Recomp}) when is_function(Decomp, 1),
                                               is_function(Recomp, 1) ->
  decomp_map_recomp(make(WorkFlow), Decomp, Recomp);
parse({map, WorkFlow, Decomp, Recomp}) when is_function(Decomp, 1),
                                            is_function(Recomp, 1) ->
  decomp_map_recomp(make(WorkFlow), Decomp, Recomp);
parse({reduce, Reduce, Decomp}) when is_function(Reduce, 2),
                                     is_function(Decomp, 1) ->
  fun(Input) ->
    Parts = Decomp(Input),
    sk_reduce:fold1(Reduce, Parts)
  end;
parse({feedback, WorkFlow, FilterFun}) when is_function(FilterFun, 1) ->
  WorkFlowFun = make(WorkFlow),
  fun(Input) ->
    feedback(WorkFlowFun, FilterFun, Input)
  end.

-spec decomp_map_recomp(worker_fun(), decomp_fun(), recomp_fun()) -> worker_fun().
decomp_map_recomp(WorkFlowFun, Decomp, Recomp) ->
  fun(Input) ->
    Recomp([WorkFlowFun(Part) || Part <- Decomp(Input)])
  end.

-spec feedback(worker_fun(), filter_fun(), any()) -> any().
feedback(WorkFlowFun, FilterFun, Input) ->
  Input1 = WorkFlowFun(Input),
  case FilterFun(Input1) of
    true -> feedback(WorkFlowFun, FilterFun, Input1);
    false -> Input1
  end.
