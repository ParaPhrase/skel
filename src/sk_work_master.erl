%%%----------------------------------------------------------------------------
%%% @author Ramsay Taylor <r.g.taylor@sheffield.ac.uk>
%%%
%%% @doc This module contains the work master, which manages the worker pool
%%%
%%% The sk_work_master process will run on whichever node is first to call
%%% sk_work_master:find(). Thereafter, find() will return the PID of that process.
%%%
%%% The work master receives register requests from 'peasants' and manintains
%%% lists of free and assigned peasants. Free peasants can be reserved and released
%%% with the relevant API calls.
%%%
%%% The work master periodically checks the health of peasants and removes 'dead'
%%% ones from the lists.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_work_master).

-export([reserve/3,release/1,release_when_idle/1,register/1,remove/1,find/0,loop/2,status/0,stop/0]).

-include("skel.hrl").

-spec start() -> pid().
start() ->
    PID = spawn(?MODULE,loop,[[],[]]),
    yes = global:register_name(sk_work_master,PID),
    PID.

-spec find() -> pid().
find() ->
    case global:whereis_name(sk_work_master) of
	undefined ->
	    start();
	PID ->
	    PID
    end.

-spec loop(list(pid),list(pid)) -> ok.
loop(Free,Assigned) ->
    receive
	{reserve,NWorkers,Workflow,CollectorPID,From} ->
	    {CleanFree,CleanAssigned} = cleanup_dead(Free,Assigned),
	    if length(CleanFree) >= NWorkers ->
		    {Workers, NewFree} = lists:split(NWorkers,CleanFree),
		    lists:map(fun(W) -> W ! {start,Workflow,CollectorPID} end,Workers),
		    From ! {ok,Workers},
		    io:format("Reserving ~p workers, now ~p Free, ~p Assigned~n",[length(Workers),length(NewFree), length(Workers) + length(CleanAssigned)]),
		    loop(NewFree,CleanAssigned ++ Workers);
	       true ->
		    io:format("Requested ~p workers but only ~p available~n",[NWorkers,length(Free)]),
		    From ! {not_enough_workers,length(Free)},
		    loop(CleanFree,CleanAssigned)
	    end;
	{release,Workers} ->
	    lists:map(fun(W) -> W ! stop end,Workers),
	    NewAssigned = lists:filter(fun(A) -> not lists:any(fun(W) -> A == W end, Workers) end, Assigned),
	    io:format("Releasing ~p workers, now ~p Free, ~p Assigned~n",[length(Workers),length(Free) + length(Workers), length(NewAssigned)]),
	    loop(Free ++ Workers, NewAssigned);
	{register,WorkerPID} ->
	    io:format("Registering new peasant ~p~n",[WorkerPID]),
	    loop([WorkerPID | Free],Assigned);
	{remove,WorkerPID} ->
	    case lists:any(fun(W) -> W == WorkerPID end, Assigned) of
		true ->
		    %% FIXME Should we attempt to re-run the task??
		    loop(lists:filter(fun(W) -> W /= WorkerPID end,Free), lists:filter(fun(W) -> W /= WorkerPID end, Assigned));
		_ ->
		    loop(lists:filter(fun(W) -> W /= WorkerPID end,Free), Assigned)
	    end;
	terminate ->
	    lists:map(fun(W) -> W ! terminate end, Free ++ Assigned),
	    ok;
%%	{job_complete,WorkerPID} ->
%%	    loop(Free,Assigned);
	{status, From} ->
	    From ! {sk_work_master_status,length(Free),length(Assigned)},
	    loop(Free,Assigned);
	Other ->
	    io:format("Unexpected message to work master:~n~p~n",[Other]),
	    loop(Free,Assigned)
    after 1000 ->
	    {NewFree,NewAssigned} = cleanup_dead(Free,Assigned),
	    loop(NewFree,NewAssigned)
    end.

cleanup_dead(Free,Assigned) ->
    States = sk_peasant:get_states(Free ++ Assigned),
    Alive = lists:foldl(fun({W,State},Acc) ->
				case State of
				    dead ->
					io:format("Worker ~p appears dead~n",[W]),
					%% FIXME Should we attempt to re-run the task if it was active??
					Acc;
				    _ ->
					[W | Acc]
				end
			end,
			[],
			States),
    {lists:filter(fun(W) -> lists:member(W,Alive) end, Free),
     lists:filter(fun(W) -> lists:member(W,Alive) end, Assigned)}.

-spec register(pid()) -> ok.
register(WorkerPID) ->
    find() ! {register,WorkerPID},
    ok.

-spec reserve(pos_integer() | {max,pos_integer()},workflow(),pid()) -> list(pid()).
reserve({max,0},_Workflow,_CollectorPID) ->
    %% This is dumb, but there might be a use for it somewhere.
    [];
reserve({max,NWorkers},Workflow,CollectorPID) ->
    find() ! {reserve,NWorkers,Workflow,CollectorPID,self()},
    receive
	{ok,Workers} ->
	    Workers;
	{not_enough_workers,Avail} ->
	    if Avail > 0 ->
		    reserve(Avail,Workflow,CollectorPID);
	       true ->
		    timer:sleep(1000),
		    reserve({max,NWorkers},Workflow,CollectorPID)
	    end
    end;
reserve(NWorkers,Workflow,CollectorPID) ->
    find() ! {reserve,NWorkers,Workflow,CollectorPID,self()},
    receive
	{ok,Workers} ->
	    Workers;
	{not_enough_workers,_Avail} ->
	    timer:sleep(1000),
	    reserve(NWorkers,Workflow,CollectorPID)
    end.

-spec release(list(pid())) -> ok.
release(Workers) ->
    find() ! {release,Workers},
    ok.

-spec release_when_idle(list(pid)) -> ok.
release_when_idle(Workers) ->
    lists:map(fun(W) -> W ! release_when_idle end, Workers),
    ok.

-spec remove(pid()) -> ok.
remove(WorkerPID) ->
    find() ! {remove,WorkerPID},
    ok.

-spec status() -> {pos_integer(),pos_integer()}.
status() ->
    sk_work_master:find() ! {status, self()},
    receive
	{sk_work_master_status,FreeN,AssignedN} ->
	    {FreeN,AssignedN}
    end.

-spec stop() -> ok.
stop() ->
    sk_work_master:find() ! terminate,
    ok.
