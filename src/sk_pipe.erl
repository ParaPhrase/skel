%%%----------------------------------------------------------------------------
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2013 University of St Andrews (See LICENCE)
%%% @doc This module contains `pipe` skeleton initialization logic.
%%%
%%% The `pipe` skeleton is just the sequential composition of several skeletons
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_pipe).

-export([
         make/1
        ]).


-ifdef(TEST).
-compile(export_all).
-endif.

-export_type([ workflow/0 ]).

-type workflow() :: { pipe, [ propertiy(), ...]}.
-type propertiy() :: { do, skel:workflow() }.



-spec make( [propertiy(), ... ] ) -> skel:maker_fun().
make( _Proplist = [ {do, WorkFlow }] ) ->
  make( WorkFlow  );
  
make(WorkFlow) ->
  fun(NextPid) ->
    sk_assembler:make(WorkFlow, NextPid)
  end.
