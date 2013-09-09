%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains `ord` skeleton reordering logic.
%%%
%%% The `ord` skeleton can reorder outputs from its inner skeletons such that
%%% they have the same order coming out the ord skeleton as they had going into
%%% it.
%%%
%%% The reorderer takes each output from the inner skeleton and only forwards 
%%% it if it has already forwarded all outputs with smaller tags.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_ord_reorderer).

-export([
         start/1
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-spec start(pid()) -> 'eos'.
start(NextPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, []),
  loop(0, dict:new(), NextPid).

-spec loop(non_neg_integer(), dict(), pid()) -> 'eos'.
loop(Seen, Dict, NextPid) ->
  receive
    {data, _, _} = DataMessage ->
      {{ord, Idx}, DataMessage1} = sk_data:pop(DataMessage),
      Dict1 = store(Idx, DataMessage1, Dict),
      {Seen1, Dict2} = forward(Seen, Dict1, NextPid),
      loop(Seen1, Dict2, NextPid);
    {system, eos} ->
      sk_tracer:t(75, self(), NextPid, {?MODULE, system}, [{message, eos}]),
      NextPid ! {system, eos},
      eos
  end.

-spec store(pos_integer(), skel:data_message(), dict()) -> dict().
store(Idx, DataMessage, Dict) ->
  dict:store(Idx, DataMessage, Dict).

-spec forward(non_neg_integer(), dict(), pid()) -> {non_neg_integer(), dict()}.
forward(Seen, Dict, NextPid) ->
  case dict:find(Seen+1, Dict) of
    error ->
      {Seen, Dict};
    {ok, DataMessage} ->
      sk_tracer:t(50, self(), NextPid, {?MODULE, data}, [{output, DataMessage}]),
      NextPid ! DataMessage,
      Dict1 = dict:erase(Seen+1, Dict),
      forward(Seen+1, Dict1, NextPid)
  end.
