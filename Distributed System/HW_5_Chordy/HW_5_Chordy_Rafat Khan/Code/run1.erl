%% @author rafat
%% @doc @todo Add description to run1.

-module(run1).
-export([start/0, stop/1]).


start() ->
    start(5).

start(N) ->
    Spid = node1:start(key:generate(),nil),
    start(N-1,Spid).

start(N,Spid) when N > 0 ->
    node1:start(key:generate(),Spid),
    start(N-1,Spid);

start(_,Pid) -> Pid.
    
stop(Nodes) ->
    lists:foreach(
        fun(Node) -> Node ! stop end,
        Nodes
    ).