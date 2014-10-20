%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% 
%%% @doc This module contains the sink logic.
%%%
%%% A sink is a process that accepts inputs off the final output stream in a 
%%% skeleton workflow. 
%%%
%%% Two kinds of sink are provided - a list accumulator sink (the default) and
%%% a module sink, that uses a callback module to deal with the data.
%%%
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_sink).

-export([
         start_acc/1
        ,start_acc/0
        ,start_mod/2
        ,start_mod/1
        ]).

-include("skel.hrl").

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



%% @doc Starts accumulator process.  Creates the process to which the
%%  final results are sent.
-spec start_acc() -> pid().
start_acc() ->
  proc_lib:spawn( ?MODULE, start_acc, [ self() ]).

-spec start_acc(pid()) -> 'eos'.
%% @doc Sets the sink process to receive messages from other processes.
start_acc(NextPid) ->
  loop_acc(NextPid, []).

-spec loop_acc(pid(), list()) -> 'eos'.
%% @doc Recursively recieves messages, collecting each result in a list. 
%% Returns the list of results when the system message <tt>eos</tt> is 
%% received. 
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



%% @doc Creates the process to which the final results are sent using the
%% specified module <tt>OutputMod</tt>.
-spec start_mod( module() ) -> pid().
start_mod( OutputMod ) ->
  proc_lib:spawn(?MODULE, start_mod, [ OutputMod, self()]).


%% @doc Initiates loop to receive messages from child processes, passing 
%% results to the given module as appropriate.
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

%% @doc Forwards the final result to the process <tt>NextPid</tt>. Returns the 
%% system message <tt>eos</tt> denoting the sink's task finished.
forward(Result, NextPid) ->
  NextPid ! {sink_results, Result},
  eos.
