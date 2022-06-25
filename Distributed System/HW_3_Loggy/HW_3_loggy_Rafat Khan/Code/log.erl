%% @author rafat
%% @doc @todo Add description to log.

-module(log).

-export([start/2, stop/1, status/1]).

start(Nodes, TimeModule) ->
    spawn_link(fun() -> init(Nodes, TimeModule) end).

stop(Logger) ->
    Logger ! stop.

init(Nodes, TimeModule) ->
    loop(TimeModule:clock(Nodes), [], TimeModule).

status(Log) ->
    Log ! {status, self()},
    receive
        {status, Pid, Clock, Queue} ->
            io:format("Log (~w) status:~nClock: ~w~nQueue: ~w~n", [Pid, Clock, Queue])
    after 1000 ->
        {error, unresponsive}
    end.

loop(Clock, Queue, TimeModule) ->
    receive
        {log, From, Time, Msg} ->
            UQueue = pushBack({Time, {From, Msg}}, Queue),
            UClock = TimeModule:update(From, Time, Clock),
            ProcessedQueue = processQueue(UQueue, UClock, TimeModule),
            loop(UClock, ProcessedQueue, TimeModule);

        {status, From} ->
            From ! {status, self(), Clock, Queue},
            loop(Clock, Queue, TimeModule);

        stop ->
            case Queue of
                [] ->
                    io:format("Log terminated with empty queue.~n");
                _ ->
                    io:format("Dumping queue on log termination:~n"),
                    dumpQueue(Queue)
            end
    end.

log(From, Time, Msg) when is_number(Time) ->
    io:format("Log: (~w) ~w ~p~n", [Time, From, Msg]);
log(From, _, Msg) ->
    io:format("Log: ~w, ~p~n", [From, Msg]).

dumpQueue([]) -> ok;
dumpQueue([{Time, {From, Msg}} | Rest]) ->
    log(From, Time, Msg),
    dumpQueue(Rest).

processQueue([], _, _) -> [];
processQueue(Queue, Clock, TimeModule) ->
    [{Time, {From, Msg}} | Rest] = Queue,
    case TimeModule:safe(Time, Clock) of
        true ->
            log(From, Time, Msg),
            processQueue(Rest, Clock, TimeModule);
        
        _ ->
            Queue
    end.

pushBack(Element, []) -> [Element];
pushBack({ETime, EMsg}, [{QTime, QMsg} | Rest]) ->
    case time:leq(ETime, QTime) of
        true ->
            [{ETime, EMsg}, {QTime, QMsg} | Rest];
        
        _ ->
            [{QTime, QMsg} | pushBack({ETime, EMsg}, Rest)]
    end.