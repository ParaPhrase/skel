%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains `feedback` skeleton counter logic.
%%%
%%% The `feedback` skeleton contains an inner skeleton. Inputs are repeatedly
%%% sent through the inner skeleton until the developer-defined feedback
%%% function returns false for the input.
%%%
%%% It turns out in the feedback skeleton we have to maintain counts of numbers
%%% inputs going through the inner skeleton and going through the feedback loop
%%% in order to avoid race conditions upon end-of-stream. This is the dual-
%%% semaphore that we use to maintain these counts.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_feedback_bicounter).

-export([
         start/0
        ,subscribe/1
        ,cast/2
        ]).

-export_type([
              message/0
             ]).

-ifdef(TEST).
-compile(export_all).
-endif.


-type message() :: {system, {counter, command(), pid()}}.
-type command() :: {bi_command(), bi_command()}
                 | subscribe.
-type bi_command() :: id
                    | incr
                    | decr.

-type counters() :: {number(), number()}.

-spec start() -> ok.
start() ->
  sk_tracer:t(90, self(), {?MODULE, start}, [{a, 0}, {b, 0}, {subscribers, []}]),
  loop({0, 0}, []).

-spec subscribe(pid()) -> ok.
subscribe(CounterPid) ->
  sk_tracer:t(85, self(), CounterPid, {?MODULE, system}, [{msg, counter}, {command, subscribe}]),
  CounterPid ! {system, {counter, subscribe, self()}},
  ok.

-spec cast(pid(), {bi_command(), bi_command()}) -> ok.
cast(CounterPid, Commands) ->
  sk_tracer:t(85, self(), CounterPid, {?MODULE, system}, [{msg, counter}, {command, Commands}]),
  CounterPid ! {system, {counter, Commands, self()}},
  ok.

-spec loop(counters(), [pid()]) -> ok.
loop(Counters, Subscribers) ->
  receive
    {system, {counter, subscribe, Pid}} ->
      loop(Counters, [Pid|Subscribers]);
    {system, {counter, Command, _}} ->
      Counters1 = command(Counters, Command, Subscribers),
      loop(Counters1, Subscribers);
    {system, eos} when Counters =:= {0,0} ->
      ok;
    {system, _} ->
      loop(Counters, Subscribers)
  end.

-spec command(counters(), {bi_command(), bi_command()}, [pid()]) -> counters().
command({CA,CB}, {CommandA, CommandB}, Subscribers) ->
  Counters = {execute(CA, CommandA), execute(CB, CommandB)},
  sk_tracer:t(85, self(), {?MODULE, execute}, [{command, {CommandA, CommandB}}, {old, {CA,CB}}, {new, Counters}]),
  notify_subscribers(Counters, Subscribers),
  Counters;
command(Counters, _, _) ->
  Counters.

-spec execute(number(), bi_command()) -> number().
execute(C, id) -> C;
execute(C, incr) -> C+1;
execute(C, decr) -> C-1;
execute(C, _) -> C.

-spec notify_subscribers(counters(), [pid()]) -> ok.
notify_subscribers(_, []) ->
  ok;
notify_subscribers(Counters, [Subscriber|Subscribers]) ->
  sk_tracer:t(90, self(), Subscriber, {?MODULE, system}, [{msg, counter}, {counters, Counters}]),
  Subscriber ! {system, {counter, Counters, self()}},
  notify_subscribers(Counters, Subscribers).
