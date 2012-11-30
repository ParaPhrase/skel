%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This module contains utility functions for tracing the execution of
%%% skel programs.
%%%
%%% Most skel processes will emit events when they send messages, allowing 
%%% the tracer to view them in a nice way. This is by far the easiest way to
%%% debug the parallelism, rather than `printf` statements.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_tracer).

-export([
         start/0
        ,report_event/4 ,t/4
        ,report_event/5 ,t/5
        ]).

-include("skel.hrl").

-compile(nowarn_unused_vars).

-spec start() -> 'ignore' | {'error',_} | {'ok',pid()}.
start() ->
  et_viewer:start([
                   {detail_level, max},
                   {trace_pattern, {sk_tracer, max}},
                   {trace_global, true},
                   {scale, 2},
                   {max_actors, infinity},
                   {width, 1000},
                   {height, 800}
                  ]).

-type detail_level() :: 1..100.
-type actor() :: term().
-type label() :: atom() | string() | term().
-type contents() :: [{term(), term()}] | term().

-spec report_event(detail_level(), actor(), actor(), label(), contents()) -> 'hopefully_traced'.
report_event(DetailLevel, From, To, Label, Contents) ->
  hopefully_traced.

-spec t(detail_level(), actor(), actor(), label(), contents()) -> 'hopefully_traced'.
t(DetailLevel, From, To, Label, Contents) ->
  ?MODULE:report_event(DetailLevel, From, To, Label, Contents).

-spec report_event(detail_level(), actor(), label(), contents()) -> 'hopefully_traced'.
report_event(DetailLevel, FromTo, Label, Contents) ->
  ?MODULE:report_event(DetailLevel, FromTo, FromTo, Label, Contents).

-spec t(detail_level(), actor(), label(), contents()) -> 'hopefully_traced'.
t(DetailLevel, FromTo, Label, Contents) ->
  ?MODULE:report_event(DetailLevel, FromTo, FromTo, Label, Contents).
