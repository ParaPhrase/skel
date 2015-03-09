%%%----------------------------------------------------------------------------
%%% @author Ramsay Taylor <r.g.taylor@sheffield.ac.uk>
%%%
%%% @doc This module contains code for peasants - workers in the worker pool
%%%
%%% A peasant registers itself with the sk_work_master global process. Then it
%%% can be assigned as needed. When sent a 'start' message it will spawn a process
%%% to execute a skeleton and it will forward data messages to that skeleton. 
%%%
%%% The peasant process itself can report on the status of the skeleton process
%%% and monitors for termination.
%%%
%%% @end
%%%----------------------------------------------------------------------------
-module(sk_peasant).

-export([start/0,loop/1,work_team/1,get_states/1,report_completion/1]).

-spec start() -> pid().
start() ->
    PID = spawn(?MODULE,loop,[idle]),
    sk_work_master:register(PID),
    PID.

-spec loop(idle) -> ok.
loop(idle) ->
    receive 
	{start,Workflow,collector_to_follow} ->
	    loop(Workflow,collector_to_follow);
	{start,Workflow,CollectorPID} ->
	    process_flag(trap_exit, true),
	    SubWorker = sk_utils:start_worker([{pipe,Workflow++[{seq,fun ?MODULE:report_completion/1}]}],self()),
	    monitor(process,SubWorker),
	    loop(Workflow,SubWorker,start,CollectorPID);
	status ->
	    sk_work_master:find() ! {status_report,self(),idle},
	    loop(idle);
	{status,From} ->
	    From ! {status_report,self(),idle},
	    loop(idle);
	terminate ->
	    ok;
	{'DOWN', _MonitorRef, _Type, _Object, _Info} ->
	    %%io:format("DOWN: ~p",[{_MonitorRef, _Type, _Object, _Info}]),
	    loop(idle);
	{'EXIT', Object, Info} ->
	    io:format("EXIT: ~p",[{Object, Info}]),
	    loop(idle);
	release_when_idle ->
	    sk_work_master:find() ! {release,[self()]},
	    loop(idle)
    end.

loop(Workflow,collector_to_follow) ->
    receive
	{set_collector,CollectorPID} ->
	    process_flag(trap_exit, true),
	    SubWorker = sk_utils:start_worker([{pipe,Workflow++[{seq,fun ?MODULE:report_completion/1}]}],self()),
	    monitor(process,SubWorker),
	    loop(Workflow,SubWorker,start,CollectorPID)
    end.

loop(Workflow,SubWorker,Last,CollectorPID) ->
    receive
	status ->
	    sk_work_master:find() ! {status_report,self(),{working,Workflow,Last}},
	    loop(Workflow,SubWorker,Last,CollectorPID);
	{status,From} ->
	    From ! {status_report,self(),{working,Workflow,Last}},
	    loop(Workflow,SubWorker,Last,CollectorPID);
	terminate ->
	    SubWorker ! {system, eos},
	    ok;
%%	{'DOWN', _, _, SubWorker, _} ->
%%	    io:format("[~p] Worker Finished ~p~n",[self(),SubWorker]),
%%	    sk_work_master:find() ! {job_complete,self()},
%%	    loop(idle);
	{'EXIT', SubWorker, Info} ->
	    io:format("[~p] Worker EXIT: ~p",[self(),{SubWorker, Info}]),
	    sk_work_master:find() ! {worker_error,self(),Info},
	    loop(idle);
	{'EXIT', OtherPid, Info} ->
	    io:format("[~p] Unexpected EXIT: ~p",[self(),{OtherPid, Info}]),
	    loop(Workflow,SubWorker,Last,CollectorPID);
	{data,{job_complete,V},Other} ->
	    CollectorPID ! {data,{complete,self(),V},Other},
	    loop(Workflow,SubWorker,Last,CollectorPID);
	{data,_,_} = DataMessage ->
	    SubWorker ! DataMessage,
	    loop(Workflow,SubWorker,DataMessage,CollectorPID);
	{system,eos} ->
	    case Last of
		{system,eos} ->
		    %% This is the echo from the sub worker
		    %% We need to forward it on and become idle
		    CollectorPID ! {system,eos},
		    loop(idle);
		_ ->
		    SubWorker ! {system,eos},
		    loop(Workflow,SubWorker,{system,eos},CollectorPID)
	    end;
	{system,_} = SystemMessage ->
	    SubWorker ! SystemMessage,
	    loop(Workflow,SubWorker,SystemMessage,CollectorPID)
    end.

-spec work_team(list(string())) -> ok.
work_team([NumString]) ->
    net_adm:world(),
    %% Force names to propagate?...
    timer:sleep(1000),
    _Master = global:whereis_name(sk_work_master),
    io:format("Master: ~p~n",[_Master]),
    {N,_} = string:to_integer(atom_to_list(NumString)),
    io:format("~nStarting ~p workers...~n",[N]),
    Peasants = start_work_team(N,[]),
    monitor_work_team(Peasants).

start_work_team(N,Team) when N < 1 ->
    Team;
start_work_team(N,Team) ->
    start_work_team(N-1,[start() | Team]).

monitor_work_team([]) ->
    ok;
monitor_work_team(Workers) ->
    timer:sleep(5000),
    States = get_states(Workers),
    io:format("------ Status ------~n"),
    monitor_work_team(lists:foldl(fun({W,State},Acc) ->  
					  io:format("~p :: ~p~n",[W,State]),
					  case State of
					      dead ->
						  sk_work_master:remove(W),
						  Acc;
					      _ ->
						  [W | Acc]
					  end
				  end,
				  [],
				  States)).

-spec get_states(list(pid())) -> list({pid(),any()}).
get_states(Workers) ->
    lists:map(fun(W) -> W ! {status,self()} end,Workers),
    collect_states(Workers,[]).

collect_states([],States) ->
    States;
collect_states(Workers,States) ->
    receive
	{status_report,W,State} ->
	    collect_states(lists:filter(fun(OW) -> OW /= W end,Workers),[{W,State} | States])
    after 5000 ->
	    lists:map(fun(W) -> {W,dead} end, Workers) ++ States
    end.

-spec report_completion(any()) -> {job_complete,any()}.
report_completion(V) ->
    {job_complete,V}.
