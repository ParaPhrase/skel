%%%----------------------------------------------------------------------------
%%% @hidden
%%% @author Sam Elliott <ashe@st-andrews.ac.uk>
%%% @copyright 2012 University of St Andrews (See LICENCE)
%%% @doc This is the start of a generic way to specify sk processes using 
%%% callbacks. 
%%%
%%% It's completely unfinished.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(gen_sk).

-include("skel.hrl").

-ifdef(TEST).
-compile(export_all).
-endif.

-callback init(Args :: term()) ->
  {ok, State :: term()}.

-callback handle_data(Message :: data_message(), State :: term()) ->
  {forward, Message1 :: [ data_message(),...], NewState :: term()} |
  {eos, NewState :: term()}.

-callback handle_system(Message :: data_message(), State :: term()) ->
  {forward, NewState :: term()} |
  {drop, NewState :: term()} |
  {eos, NewState :: term()}.

-callback handle_info(Info :: term(), State :: term()) ->
  {ok, NewState :: term()} |
  {eos, NewState :: term()}.

-callback terminate(State :: term()) ->
  ok.

