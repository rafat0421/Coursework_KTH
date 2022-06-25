%% @author rafat
%% @doc @todo Add description to test2.

-module(map).
-export([new/0,update/3,reachable/2,all_nodes/1]).

%return an empty map
new()->
    [].

%update the map with a new node
update(Node,Links,Map)->
    lists:keydelete(Node,1,Map),
    lists:append(Map,[{Node,Links}]).

%find all reachable cities of the node according to the map
reachable(Node,Map)->
    NodeMap = lists:keyfind(Node,1,Map),
    if
        NodeMap == false ->
          [];
        NodeMap /= false->
              {_, ReachableCities} = NodeMap,
            ReachableCities
    end.

%get all nodes in the map in a list(without duplicate ones)
all_nodes(Map)->
    Temp = go_through(Map,[]),
    %delete the duplicate nodes
    lists:usort(lists:flatten(Temp)).

%tranverse tuple to list
tranverse(Element)->
    if 
        erlang:is_tuple(Element) == true->
            T_Element = tuple_to_list(Element)          
    end,
T_Element.


%go through all elements in the list
go_through([H|T],Result) -> 
    H1 = H,
    Changed_H1= tranverse(H1),
    go_through(T,[lists:flatten(Changed_H1)|Result]);

go_through([],Result) -> Result.