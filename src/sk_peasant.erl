-module(sk_peasant).

-export([start/0,loop/1]).

start() ->
    PID = spawn(?MODULE,loop,[idle]),
    sk_work_master:register(PID),
    PID.

loop(idle) ->
    receive 
	{start,Workflow,collector_to_follow} ->
	    loop({Workflow,collector_to_follow});
	{start,Workflow,CollectorPID} ->
	    SubWorker = sk_utils:start_worker(Workflow,CollectorPID),
	    io:format("~p Starting ~p as ~p [output to ~p]~n",[self(),Workflow,SubWorker,CollectorPID]),
	    loop({Workflow,SubWorker});
	status ->
	    sk_work_master:find() ! {status_report,self(),idle};
	terminate ->
	    ok
    end;
loop({Workflow,collector_to_follow}) ->
    receive
	{set_collector,CollectorPID} ->
	    SubWorker = sk_utils:start_worker(Workflow,CollectorPID),
	    loop({Workflow,SubWorker})
    end;
loop({Workflow,SubWorker}) ->
    receive
	status ->
	    sk_work_master:find() ! {status_report,self(),{working,Workflow}};
	terminate ->
	    SubWorker ! {system, eos},
	    ok;
	{system, eos} ->
	    SubWorker ! {system, eos},
	    loop(idle);
	Msg ->
	    io:format("Passing on ~p~n",[Msg]),
	    SubWorker ! Msg,
	    loop({Workflow,SubWorker})
    end.
