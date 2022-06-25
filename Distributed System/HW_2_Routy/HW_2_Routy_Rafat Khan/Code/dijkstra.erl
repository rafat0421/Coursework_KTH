%% @author rafat
%% @doc @todo Add description to test2.

-module(dijkstra).
-export([table/2,route/2,update/4,iterate/3]).

%return the length of shortest path to the node
%if node is not found, then return 0
entry(Node,Sorted)->
    Temp = lists:keyfind(Node, 1, Sorted),
  
    if 
        %to make sure that if there is no such Node in the list,
        %then the new entry will not be added
        Temp == false->
        0;
        true->
            Temp1 = lists:filter(fun({E,_,_}) -> E == Node end, Sorted),
            Result = lists:foldl(fun compare/2, lists:nth(1,Temp1), Temp1),
            erlang:element(2,Result)
    end.

%return the element with shorter path
compare(X,Y)->
    if 
        erlang:element(2,X) > erlang:element(2,Y)->
            Y;
        true->
            X
        end.

%replace the node in the sorted list with the new node 
replace(Node,N,Gateway,Sorted)->
    NewSorted = lists:keydelete(Node,1,Sorted),
    lists:keymerge(2, NewSorted, [{Node,N,Gateway}]).

%update the sorted list
%if the new path is shorter, then replace the original node element
update(Node,N,Gateway,Sorted)->
    Temp2 = entry(Node,Sorted),
      if 
        Temp2 > N ->
            replace(Node,N,Gateway,Sorted);
        true->
            Sorted
    end.

%iterate with dijkstra algorithm
iterate([], _, Table) ->
    Table;
iterate([{_, inf, _} | _], _, Table) ->
    Table;
iterate([{Node, N, Gateway} | Rest], Map, Table) ->
    ReachableCities = map:reachable(Node, Map),
    UpdatedSorted = lists:foldl(fun(ReachableCity, List) -> update(ReachableCity, N + 1, Gateway, List) end, Rest, ReachableCities),
    iterate(UpdatedSorted, Map, [{Node,Gateway}|Table]).


%generate a table with given gateways and map
table(Gateways,Map)->
    Map_Nodes = map:all_nodes(Map),
    Dummy_entries = lists:map(fun(Node)->
        case lists:member(Node,Gateways) of
            true->
                {Node,0,Node};
            false->
                {Node,inf,unknown}
            end
        end,Map_Nodes),
    IniSorted = lists:keysort(2, Dummy_entries),
    iterate(IniSorted,Map,[]).

%find the gateway city to send the message from node
route(_Node,[])->
    notfound;
route(Node,[{City,Gateway}|Rest])->
     case Node == City of
            true->
                {ok,Gateway};
            false->
                route(Node,Rest)
    end.