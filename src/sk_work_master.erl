-module(sk_work_master).

-export([reserve/3,release/1,register/1,remove/1,find/0,loop/2]).

start() ->
    PID = spawn(?MODULE,loop,[[],[]]),
    yes = global:register_name(sk_work_master,PID),
    PID.

find() ->
    case global:whereis_name(sk_work_master) of
	undefined ->
	    start();
	PID ->
	    PID
    end.

loop(Free,Assigned) ->
    receive
	{reserve,NWorkers,Workflow,CollectorPID,From} ->
	    if length(Free) >= NWorkers ->
		    {Workers, NewFree} = lists:split(NWorkers,Free),
		    lists:map(fun(W) -> W ! {start,Workflow,CollectorPID} end,Workers),
		    From ! {ok,Workers},
		    io:format("Reserving ~p workers, now ~p Free, ~p Assigned~n",[length(Workers),length(NewFree), length(Workers) + length(Assigned)]),
		    loop(NewFree,Assigned ++ Workers);
	       true ->
		    io:format("Requested ~p workers but only ~p available~n",[NWorkers,length(Free)]),
		    From ! not_enough_workers,
		    loop(Free,Assigned)
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
	Other ->
	    io:format("Unexpected message to work master:~n~p~n",[Other]),
	    loop(Free,Assigned)
    end.

register(WorkerPID) ->
    find() ! {register,WorkerPID}.

reserve(NWorkers,Workflow,CollectorPID) ->
    io:format("Attempting to reserve ~p workers from ~p~n~p~n~p~n",[NWorkers,find(),Workflow,CollectorPID]),
    find() ! {reserve,NWorkers,Workflow,CollectorPID,self()},
    receive
	{ok,Workers} ->
	    Workers;
	not_enough_workers ->
	    timer:sleep(1000),
	    reserve(NWorkers,Workflow,CollectorPID)
    end.

release(Workers) ->
    find() ! {release,Workers}.

remove(WorkerPID) ->
    find() ! {remove,WorkerPID}.
