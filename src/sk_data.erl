%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%%
%%% @doc This module encapsulates all functions relating to data messages that
%%% are sent around the system.
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

-spec fmap(fun((any()) -> any())) -> data_fun().
%% @doc Allows a given function to be applied to a value in the form of a Skel 
%% data message. Preserves message identifiers.
fmap(Fun) ->
  fun ({data, Value, Ids}) ->
    {data, Fun(Value), Ids}
  end.

-spec pure(any()) -> data_message().
%% @doc Formats a piece of data under `D' of any data type as a data message.
pure(D) ->
  {data, D, []}.

-spec apply(data_message()) -> data_fun().
%% @doc Allows a given function, formatted as a data message, to be applied to 
%% a value in the form of a Skel data message. Preserves message identifiers.
apply({data, Fun, _}) ->
  fmap(Fun).

-spec value(data_message()) -> any().
%% @doc Extracts and returns a data message's value.
value({data, Value, _Identifiers}) ->
  Value.

-spec push(dm_identifier(), data_message()) -> data_message().
%% @doc Adds an identifier to a given data message.
push(Identifier, {data, Value, Identifiers}) ->
  {data, Value, [Identifier|Identifiers]}.

-spec pop(data_message()) -> {dm_identifier(), data_message()}.
%% @doc Retrieves and removes the topmost identifier of a given message. 
%% Returns a tuple containing retrieved identifier, and the now-updated 
%% message.
pop({data, Value, [Identifier|Identifiers]}) ->
  DM = {data, Value, Identifiers},
  {Identifier, DM}.

-spec peek(data_message()) -> {ok, dm_identifier()} | empty.
%% @doc Retrieves, but does not remove, the topmost identifier of a given message. Returns only the identifier, or empty when no identifiers exist.
peek({data,_Value, []}) ->
  empty;
peek({data,_Value, [Identifier|_]}) ->
  {ok, Identifier}.

-spec decomp_by(decomp_fun()) -> data_decomp_fun().
%% @doc Standardises the format of the developer-defined decomposition function under `DecompFun' for use in Skel.
decomp_by(DecompFun) ->
  fun({data, Value, Ids}) ->
    Partitions = DecompFun(Value),
    [{data, X, Ids} || X <- Partitions]
  end.

-spec recomp_with(recomp_fun()) -> data_recomp_fun().
%% @doc Standardises the format of the developer-defined recomposition function under `RecompFun' for use in Skel.
recomp_with(RecompFun) ->
  fun([{data, _, Ids}|_] = DataMessages) ->
    Values = [value(X) || X <- DataMessages],
    {data, RecompFun(Values), Ids}
  end.

-spec reduce_with(reduce_fun()) -> data_reduce_fun().
%% @doc Standardises the format of the developer-defined reduction function under `Reduce' for use in Skel.
reduce_with(Reduce) ->
  fun({data, _, Ids} = DataMessage1, DataMessage2) ->
    {data, Reduce(value(DataMessage1), value(DataMessage2)), Ids}
  end.

-spec filter_with(filter_fun()) -> data_filter_fun().
%% @doc Standardises the format of teh developer-defined constraint-checking function under `Filter' for use in Skel.
filter_with(Filter) ->
  fun({data, _, _} = DataMessage1) ->
    Filter(value(DataMessage1))
  end.
