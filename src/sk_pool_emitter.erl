%%%----------------------------------------------------------------------------
%%% @author Ramsay Taylor <r.g.taylor@sheffield.ac.uk>
%%%
%%% @doc This module contains the emitter for the pool skeleton.
%%%
%%% This emitter retains most of the data and releases one item to each worker
%%% when the sk_pool_collector reports the recipt of the previous result from
%%% that worker.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_pool_emitter).
-export([start/1]).

-include("skel.hrl").

-spec start([pid(),...]) -> 'eos'.
start(Workers) ->
    sk_tracer:t(75, self(), {?MODULE, start}, [{workers, Workers}]),
    loop(Workers,[],[]).

-spec loop(list(pid()), list(pid()), list()) -> 'eos'.
loop(Idle,Busy,Work) ->
    receive
	{data, _, _} = DataMessage ->
	    %% Clear up any workers that have died whilst waiting (usually killed by someone else)
	    States = sk_peasant:get_states(Idle),
	    TrueIdle = lists:foldl(fun({W,S},Acc) ->
					   case S of
					       dead -> Acc;
					       _ -> Acc ++ [W]
					   end
				   end, [], States),
	    case TrueIdle of
		[] ->
		    loop(Idle,Busy,Work ++ [DataMessage]);
		[I | Is] ->
		    sk_tracer:t(50, self(), I, {?MODULE, data}, [{input, DataMessage}]),
		    I ! DataMessage,
		    loop(Is,[I | Busy],Work)
	    end;
	{system,eos} ->
	    lists:map(fun(W) -> W ! {system,eos} end, Idle),
	    sk_work_master:release(Idle),
	    loop([], Busy, Work ++ [{system,eos}]);
	{complete,WPID} ->
	    case Work of
		[] ->
		    loop([WPID | Idle], lists:filter(fun(W) -> W /= WPID end, Busy), Work);
		[{system,eos}] ->
		    WPID ! {system,eos},
		    sk_work_master:release([WPID]),
		    case Busy of
			[] ->
			    ok;
			_ ->
			    loop([],Busy,Work)
		    end;
		[D | MoreWork] ->
		    sk_tracer:t(50, self(), WPID, {?MODULE, data}, [{input, D}]),
		    WPID ! D,
		    loop(Idle,Busy,MoreWork)
	    end
	end.
