%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains `ord` skeleton tagger logic.
%%%
%%% The `ord` skeleton can reorder outputs from its inner skeletons such that
%%% they have the same order coming out the ord skeleton as they had going into
%%% it.
%%%
%%% The tagger takes each input and adds information as to its place in the 
%%% input stream of the inner skeleton.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_ord_tagger).

-export([
         start/1
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-spec start(pid()) -> 'eos'.
start(NextPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, []),
  loop(1, NextPid).

-spec loop(pos_integer(), pid()) -> 'eos'.
loop(Idx, NextPid) ->
  receive
    {data, _, _} = DataMessage ->
      DataMessage1 = sk_data:push({ord, Idx}, DataMessage),
      sk_tracer:t(50, self(), NextPid, {?MODULE, data}, [{input, DataMessage}, {output, DataMessage1}]),
      NextPid ! DataMessage1,
      loop(Idx+1, NextPid);
    {system, eos} ->
      sk_tracer:t(75, self(), NextPid, {?MODULE, system}, [{message, eos}]),
      NextPid ! {system, eos},
      eos
  end.
