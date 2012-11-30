%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module encapsulates all the functions relating to data messages
%%% that get sent around the system.
%%%
%%% Data messages are an instance of an applicative functor, allowing various
%%% operations to happen in a very logical way.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_data).

-export([
         fmap/1
        ,pure/1
        ,apply/1
        ,value/1
        ,push/2
        ,pop/1
        ,peek/1
        ,decomp_by/1
        ,recomp_with/1
        ,reduce_with/1
        ,filter_with/1
        ]).

-include("skel.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-spec fmap(fun((any()) -> any())) -> skel:data_fun().
fmap(Fun) ->
  fun ({data, Value, Ids}) ->
    {data, Fun(Value), Ids}
  end.

-spec pure(any()) -> skel:data_message().
pure(D) ->
  {data, D, []}.

-spec apply(skel:data_message()) -> skel:data_fun().
apply({data, Fun, _}) ->
  fmap(Fun).

-spec value(skel:data_message()) -> any().
value({data, Value, _Identifiers}) ->
  Value.

-spec push(skel:dm_identifier(), skel:data_message()) -> skel:data_message().
push(Identifier, {data, Value, Identifiers}) ->
  {data, Value, [Identifier|Identifiers]}.

-spec pop(skel:data_message()) -> {skel:dm_identifier(), skel:data_message()}.
pop({data, Value, [Identifier|Identifiers]}) ->
  DM = {data, Value, Identifiers},
  {Identifier, DM}.

-spec peek(skel:data_message()) -> {ok, skel:dm_identifier()} | empty.
peek({data,_Value, []}) ->
  empty;
peek({data,_Value, [Identifier|_]}) ->
  {ok, Identifier}.

-spec decomp_by(skel:decomp_fun()) -> skel:data_decomp_fun().
decomp_by(DecompFun) ->
  fun({data, Value, Ids}) ->
    Partitions = DecompFun(Value),
    [{data, X, Ids} || X <- Partitions]
  end.

-spec recomp_with(skel:recomp_fun()) -> skel:data_recomp_fun().
recomp_with(RecompFun) ->
  fun([{data, _, Ids}|_] = DataMessages) ->
    Values = [value(X) || X <- DataMessages],
    {data, RecompFun(Values), Ids}
  end.

-spec reduce_with(skel:reduce_fun()) -> skel:data_reduce_fun().
reduce_with(Reduce) ->
  fun({data, _, Ids} = DataMessage1, DataMessage2) ->
    {data, Reduce(value(DataMessage1), value(DataMessage2)), Ids}
  end.

-spec filter_with(skel:filter_fun()) -> skel:data_filter_fun().
filter_with(Filter) ->
  fun({data, _, _} = DataMessage1) ->
    Filter(value(DataMessage1))
  end.
