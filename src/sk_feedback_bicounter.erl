%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% 
%%% @doc This module contains 'feedback' skeleton counter logic.
%%%
%%% The Feedback skeleton repeatedly passes output from its inner-workflow 
%%% back into said workflow until a given constraint-checking function fails.
%%%
%%% It turns out in the feedback skeleton we have to maintain counts of numbers
%%% inputs going through the inner skeleton and going through the feedback loop
%%% in order to avoid race conditions upon end-of-stream. This is the dual-
%%% semaphore that we use to maintain these counts.
%%%
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

-include("skel.hrl").


-type counters()    :: {number(), number()}.
-type message()     :: {system, {counter, command(), pid()}}.
-type command()     :: {bi_command(), bi_command()}
                     | subscribe.
-type bi_command()  :: id
                     | incr
                     | decr.

-spec start() -> ok.
%% @doc Initialises a counter.
start() ->
  sk_tracer:t(90, self(), {?MODULE, start}, [{a, 0}, {b, 0}, {subscribers, []}]),
  loop({0, 0}, []).

-spec subscribe(pid()) -> ok.
%% @doc Subscribes the calling process to the counter at `CounterPid'. 
%% Subscribed processes will receive messages from the counter when it updates.
subscribe(CounterPid) ->
  sk_tracer:t(85, self(), CounterPid, {?MODULE, system}, [{msg, counter}, {command, subscribe}]),
  CounterPid ! {system, {counter, subscribe, self()}},
  ok.

-spec cast(pid(), {bi_command(), bi_command()}) -> ok.
%% @doc The commands given by a process under `Commands' are sent to be 
%% executed by the counter at `CounterPid'.
cast(CounterPid, Commands) ->
  sk_tracer:t(85, self(), CounterPid, {?MODULE, system}, [{msg, counter}, {command, Commands}]),
  CounterPid ! {system, {counter, Commands, self()}},
  ok.

-spec loop(counters(), [pid()]) -> ok.
%% @doc Recursively receives commands to update the counter tuple under 
%% `Counters'.
%% 
%% Adds a subsriber to the list under `Subscribers', executes a given command, 
%% and halts the counter when both the `eos' system message is received and 
%% the counters are both zero.
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
%% @doc Executes a given command for the specified counters, notifying 
%% subscribers of the change.
command({CA,CB}, {CommandA, CommandB}, Subscribers) ->
  Counters = {execute(CA, CommandA), execute(CB, CommandB)},
  sk_tracer:t(85, self(), {?MODULE, execute}, [{command, {CommandA, CommandB}}, {old, {CA,CB}}, {new, Counters}]),
  notify_subscribers(Counters, Subscribers),
  Counters;
command(Counters, _, _) ->
  Counters.

-spec execute(number(), bi_command()) -> number().
%% @doc Evaluates an individual command given the counter part `C'.
execute(C, id) -> C;
execute(C, incr) -> C+1;
execute(C, decr) -> C-1;
execute(C, _) -> C.

-spec notify_subscribers(counters(), [pid()]) -> ok.
%% @doc Recursively sends the updated counter under `Counters' to each 
%% subscriber in the attached list of subscribers.
notify_subscribers(_, []) ->
  ok;
notify_subscribers(Counters, [Subscriber|Subscribers]) ->
  sk_tracer:t(90, self(), Subscriber, {?MODULE, system}, [{msg, counter}, {counters, Counters}]),
  Subscriber ! {system, {counter, Counters, self()}},
  notify_subscribers(Counters, Subscribers).
