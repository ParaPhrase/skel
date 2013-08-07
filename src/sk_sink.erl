%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains the sink logic.
%%%
%%% A sink is a process that accepts inputs off the final output stream in a 
%%% skeleton workflow. 
%%%
%%% Two kinds of sinks are provided - a list accumulator sink (the default) and
%%% a module sink, that uses a callback module to deal with the data.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_sink).

-export([
         make/0
        ,start_acc/1
        ,make/1
        ,make/2
        ,start_mod/2
        ]).

-ifdef(TEST).
-compile(export_all).
-endif.

-callback init() ->
    {ok, State :: term()} |
    {stop, State :: term()}.

-callback next_input(NextInput :: term(), State :: term()) ->
    {ok, NewState :: term()} |
    {stop, NewState :: term()}.

-callback terminate(State :: term()) ->
    term().

-spec make() -> skel:maker_fun().
make() ->
  fun(Pid) ->
    spawn(?MODULE, start_acc, [Pid])
  end.

-spec make(module()) -> skel:maker_fun().
make(OutputMod) ->
    spawn(?MODULE, start_mod, [OutputMod, self()]).

-spec make(module(), pid()) -> skel:maker_fun().
make(OutputMod, Pid) ->
    spawn(?MODULE, start_mod, [OutputMod, Pid]).



-spec start_acc(pid()) -> 'eos'.
start_acc(NextPid) ->
  loop_acc(NextPid, []).

-spec loop_acc(pid(), list()) -> 'eos'.
loop_acc(NextPid, Results) ->
  receive
    {data, _, _} = DataMessage ->
      Value = sk_data:value(DataMessage),
      sk_tracer:t(50, self(), {?MODULE, data}, [{input, DataMessage}, {value, Value}]),
      loop_acc(NextPid, Results ++ [Value]);
    {system, eos} ->
      sk_tracer:t(75, self(), {?MODULE, system}, [{msg, eos}]),
      forward(Results, NextPid)
  end.

-spec start_mod(module(), pid()) -> 'eos'.
start_mod(OutputMod, NextPid) ->
  case OutputMod:init() of
    {ok, State} -> loop_mod(OutputMod, State, NextPid);
    {stop, State} ->
      Result = OutputMod:terminate(State),
      forward(Result, NextPid)
  end.

-spec loop_mod(module(), term(), pid()) -> 'eos'.
loop_mod(OutputMod, State, NextPid) ->
  receive
    {data, _, _} = DataMessage ->
      Value = sk_data:value(DataMessage),
      sk_tracer:t(50, self(), {?MODULE, data}, [{input, DataMessage}, {value, Value}]),
      case OutputMod:next_input(Value, State, NextPid) of
        {ok, NewState} -> loop_mod(OutputMod, NewState, NextPid);
        {stop, NewState} ->
          Result = OutputMod:terminate(NewState),
          forward(Result, NextPid)
      end;
    {system, eos} ->
      sk_tracer:t(75, self(), {?MODULE, system}, [{msg, eos}]),
      Result = OutputMod:terminate(State),
      forward(Result, NextPid)
  end.

forward(Result, NextPid) ->
  NextPid ! {sink_results, Result},
  eos.
