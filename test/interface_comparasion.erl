-module(interface_comparasion).

%% Proposition of new a intreface to skeletons.
%%
%% Current interface schows some problems with the reduce patters, as
%% there is no inner workflow (at least there is no such
%% possibility).  Similar problem will arise with new patterns, like
%% evolution pool or div and conquare.
%%
%% My solution treats all skeleton properietis (mandatory or not) as
%% proplist.  This allow to unify interface, push pattern matching to
%% right modules and incresase error messages.
%%
%% Additionaly we increase the readability of skeleton creation; which
%% is good thing, since we do not want any wrapping functions around
%% skeletons.



orginal_interface()->
  skel:do( [{ map,
              [ {farm ,
                 fun (X) -> X*3 end,
                 4 }],
              fun erlang:tuple_to_list/1,
              fun erlang:list_to_tuple/1 }] ,
           [ {0,1,2},
             {3,4,5} ] ).

current_interface()->
  skel:do( [{ map,
              [{farm,
                [fun (X) -> X*3 end],
                [{workers, 4 } ]}],
              [{decomp, fun erlang:tuple_to_list/1},
               { recomp, fun erlang:list_to_tuple/1}] }] ,
           [ {0,1,2},
             {3,4,5} ] ).

proposed_interface()->
  skel:do( [{ map, 
              [ { do, [{farm ,[ {do, fun (X) -> X*3 end },
                                {workers, 4}] }]},
                { decomp, fun erlang:tuple_to_list/1},
                { recomp, fun erlang:list_to_tuple/1}] }],
           [ {0,1,2},
             {3,4,5} ] ).

