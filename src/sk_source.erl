%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%% @doc This module contains the source logic.
%%%
%%% A source is a process that provides the given inputs to the first process
%%% in a skeleton workflow.
%%%
%%% Two kinds of sources are provided - a list source (the default) and
%%% a module source, that uses a callback module to deal with the data.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_source).

-export([
         start/2
        ,init/2
        ]).

-include("skel.hrl").


-callback init() ->
    {ok, State :: term()} |
    {no_inputs, State :: term()}.

-callback next_input(State :: term()) ->
    {input, NextInput :: term(), NewState :: term()} |
    {ignore, NewState :: term()} |
    {eos, NewState :: term()}.

-callback terminate(State :: term()) ->
    ok.


-spec start(input(), pid() | atom ()) -> pid().
start( Input, NextPid ) ->
  proc_lib:spawn( ?MODULE, init, [ Input, NextPid]).

%% @doc Transmits each input in <tt>Input</tt> to the process <tt>NextPid</tt>.
%% @todo add documentation for the callback loop
-spec init(input(), pid() | atom() ) -> 'eos'.
init(Input, NextPid) when is_list(Input) ->
  list_loop(Input, NextPid);
init(InputMod, NextPid) when is_atom(InputMod) ->
  case InputMod:init() of
    {ok, State} -> callback_loop(InputMod, State, NextPid);
    {no_inputs, State}  ->
      send_eos(NextPid),
      InputMod:terminate(State)
  end.

%% @doc Recursively sends each input in a given list to the process 
%% <tt>NextPid</tt>.
list_loop([], NextPid) ->
  send_eos(NextPid);
list_loop([Input|Inputs], NextPid) ->
  send_input(Input, NextPid),
  list_loop(Inputs, NextPid).

%% @todo doc
callback_loop(InputMod, State, NextPid) ->
  case InputMod:next_input(State) of
    {input, NextInput, NewState} ->
      send_input(NextInput, NextPid),
      callback_loop(InputMod, NewState, NextPid);
    {ignore, NewState} ->
      callback_loop(InputMod, NewState, NextPid);
    {eos, NewState} ->
      send_eos(NextPid),
      InputMod:terminate(NewState),
      eos
  end.

%% @doc <tt>Input</tt> is formatted as a data message and sent to the 
%% process <tt>NextPid</tt>. 
send_input(Input, NextPid) ->
  DataMessage = sk_data:pure(Input),
  sk_tracer:t(50, self(), NextPid, {?MODULE, data}, [{output, DataMessage}]),
  NextPid ! DataMessage.

send_eos(NextPid) ->
  sk_tracer:t(75, self(), NextPid, {?MODULE, system}, [{msg, eos}]),
  NextPid ! {system, eos},
  eos.


