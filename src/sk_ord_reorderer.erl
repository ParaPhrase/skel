%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% 
%%% @doc This module contains 'ord' skeleton reordering logic.
%%%
%%% The 'ord' skeleton can reorder outputs from its inner skeletons such that
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

-include("skel.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-spec start(pid()) -> 'eos'.
%% @doc Ensures that output is given in the same order as the input received. 
%% For each message reveived as output, that message is only released when all 
%% outstanding messages have been sent.
start(NextPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, []),
  loop(0, dict:new(), NextPid).

% receive loop until eos
% opens the data message
% dict1 is a dictionary of {Identifier, Data Message} key-value pairs
% Seen is a count of the loop: how many things it has seen.

-spec loop(non_neg_integer(), dict:dict(), pid()) -> 'eos'.
%% @doc Recursively receives and stores messages until they are ready for 
%% release.
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

-spec store(pos_integer(), data_message(), dict:dict()) -> dict:dict().
%% @doc Stores the given `Idx', indicating position, and message `DataMessage' 
%% in the dictionary `Dict'. Returns the resulting dictionary.
store(Idx, DataMessage, Dict) ->
  dict:store(Idx, DataMessage, Dict).

-spec forward(non_neg_integer(), dict:dict(), pid()) -> {non_neg_integer(), dict:dict()}.
%% @doc Determines if any messages in the dictionary `Dict' can be released to 
%% the process `NextPid'. This decision is based upon which messages have 
%% already been released as indicated by the `Seen' counter. 
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
