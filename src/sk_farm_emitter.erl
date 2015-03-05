%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @headerfile "skel.hrl"
%%%
%%% @doc This module contains the emitter logic of a Farm skeleton.
%%%
%%% A task farm has the most basic kind of stream parallelism - inputs are
%%% sent to one of `n' replicas of the inner skeleton for processing.
%%%
%%% The emitter takes inputs off the skeleton's input stream and assigns each
%%% one to one of the input streams of the 'n' inner skeletons.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_farm_emitter).

-export([
         start/1
        ]).

-include("skel.hrl").

-spec start([pid(),...]) -> 'eos'.
%% @doc Initialises the emitter. Sends input as messages to the list of 
%% processes given by `Workers'.
start(Workers) ->
    start(Workers,false).

start(Workers, Pool) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{workers, Workers}]),
  loop(Workers, Pool).

-spec loop([pid(),...], boolean()) -> 'eos'.
%% @doc Inner-function to {@link start/1}; recursively captures input, sending %% that input onto the next worker in the list. Ceases only when the halt 
%% command is received.
loop([Worker|Rest] = Workers, Pool) ->
  receive
    {data, _, _} = DataMessage ->
	  io:format("Forwarding data message ~p~n",[DataMessage]),
	  sk_tracer:t(50, self(), Worker, {?MODULE, data}, [{input, DataMessage}]),
	  Worker ! DataMessage,
	  loop(Rest ++ [Worker],Pool);
    {system, eos} ->
	  if Pool ->
		  sk_work_master:release(Workers);
	     true ->
		  sk_utils:stop_workers(?MODULE, Workers)
	  end
  end.
