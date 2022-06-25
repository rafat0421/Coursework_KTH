%% @author rafat
%% @doc @todo Add description to time.

-module(time).

-export([zero/0, inc/2, merge/2, leq/2, clock/1, update/3, safe/2]).

% Return intitial time
zero() -> 0.

% Increment a time
inc(Name, T) -> T + 1.

% Merge 2 times
merge(T1, T2) -> max(T1, T2).

% Return true if T1 is less than or equal to T2
leq(T1, T2) -> T1 =< T2.

% Return a clock that can keep track of Nodes
clock(Nodes) -> clock(Nodes, #{}).

clock([], Map) -> {zero(), Map};
clock([Node | Rest], Map) -> clock(Rest, Map#{Node => zero()}).

% Return a clock that has been updated given that we have received a log message from a Node at a given Time
update(Node, Time, {_, Map}) ->
    UMap = Map#{Node => Time},
    Times = maps:values(UMap),
    UMinTime = lists:foldl(fun(NodeTime, Acc) -> min(NodeTime, Acc) end, inf, Times),
    {UMinTime, UMap}.

% Return true if an event that happened at a given Time is safe to print
safe(Time, {MinTime, _}) -> leq(Time, MinTime).