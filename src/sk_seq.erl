%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains the most logic of the most basic kind of skeleton
%%% - `seq`.
%%%
%%% A `seq` instance is a wrapper for a sequential function, taking an input
%%% from its input stream, applying the function to it and sending the result
%%% on its output stream.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_seq).

-export([
         start/2
        ,make/1
        ]).


-ifdef(TEST).
-compile(export_all).
-endif.

-spec make(skel:worker_fun())  -> skel:maker_fun().
make(WorkerFun) ->
  fun(NextPid) ->
    spawn(?MODULE, start, [WorkerFun, NextPid])
  end.

-spec start(skel:worker_fun(), pid()) -> eos.
start(WorkerFun, NextPid) ->
  sk_tracer:t(75, self(), {?MODULE, start}, [{next_pid, NextPid}]),
  DataFun = sk_data:fmap(WorkerFun),
  loop(DataFun, NextPid).

-spec loop(skel:data_fun(), pid()) -> eos.
loop(DataFun, NextPid) ->
  receive
    {data,_,_} = DataMessage ->
      DataMessage1 = DataFun(DataMessage),
      sk_tracer:t(50, self(), NextPid, {?MODULE, data}, [{input, DataMessage}, {output, DataMessage1}]),
      NextPid ! DataMessage1,
      loop(DataFun, NextPid);
    {system, eos} ->
      sk_tracer:t(75, self(), NextPid, {?MODULE, system}, [{message, eos}]),
      NextPid ! {system, eos},
      eos
  end.

