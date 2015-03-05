-module(sk_peasant).

-export([start/0,loop/1,work_team/1]).

start() ->
    PID = spawn(?MODULE,loop,[idle]),
    sk_work_master:register(PID),
    PID.

loop(idle) ->
    receive 
	{start,Workflow,collector_to_follow} ->
	    loop({Workflow,collector_to_follow,start});
	{start,Workflow,CollectorPID} ->
	    SubWorker = sk_utils:start_worker(Workflow,CollectorPID),
	    monitor(process,SubWorker),
	    io:format("~p Starting ~p as ~p [output to ~p]~n",[self(),Workflow,SubWorker,CollectorPID]),
	    loop({Workflow,SubWorker,start});
	status ->
	    sk_work_master:find() ! {status_report,self(),idle},
	    loop(idle);
	{status,From} ->
	    From ! {status_report,self(),idle},
	    loop(idle);
	terminate ->
	    ok;
	{'DOWN', MonitorRef, Type, Object, Info} ->
	    io:format("DOWN: ~p",[{MonitorRef, Type, Object, Info}]),
	    loop(idle)
    end;
loop({Workflow,collector_to_follow}) ->
    receive
	{set_collector,CollectorPID} ->
	    SubWorker = sk_utils:start_worker(Workflow,CollectorPID),
	    loop({Workflow,SubWorker})
    end;
loop({Workflow,SubWorker,Last}) ->
    receive
	status ->
	    sk_work_master:find() ! {status_report,self(),{working,Workflow,Last}},
	    loop({Workflow,SubWorker,Last});
	{status,From} ->
	    From ! {status_report,self(),{working,Workflow,Last}},
	    loop({Workflow,SubWorker,Last});
	terminate ->
	    SubWorker ! {system, eos},
	    ok;
	{'DOWN', _, _, SubWorker, _} ->
	    io:format("Worker Down ~p~n",[SubWorker]),
	    loop(idle);
	Msg ->
	    io:format("Passing on ~p~n",[Msg]),
	    SubWorker ! Msg,
	    loop({Workflow,SubWorker,Msg})
    end.

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
    lists:map(fun(W) -> W ! {status,self()} end,Workers),
    States = collect_states(Workers,[]),
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

collect_states([],States) ->
    States;
collect_states(Workers,States) ->
    receive
	{status_report,W,State} ->
	    collect_states(lists:filter(fun(OW) -> OW /= W end,Workers),[{W,State} | States])
    after 1000 ->
	    lists:map(fun(W) -> {W,dead} end, Workers) ++ States
    end.

