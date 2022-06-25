%% @author rafat
%% @doc @todo Add description to worker.

-module(worker).

-export([start/6, stop/1, peers/2]).

start(Name, Logger, Seed, Sleep, Jitter, TimeModule) ->
    spawn_link(fun() -> init(Name, Logger, Seed, Sleep, Jitter, TimeModule) end).

stop(Worker) ->
    Worker ! stop.

peers(Worker, Peers) ->
    Worker ! {peers, Peers}.

init(Name, Logger, Seed, Sleep, Jitter, TimeModule) ->
    rand:seed(exsp, {Seed, Seed, Seed}),
    receive
        {peers, Peers} ->
            loop(Name, Logger, Peers, TimeModule:zero(), Sleep, Jitter, TimeModule);
        
        stop ->
            ok
    end.

loop(Name, Log, Peers, Time, Sleep, Jitter, TimeModule) ->
    Wait = rand:uniform(Sleep),
    receive
        {msg, MsgTime, Msg} ->
            NewTime = TimeModule:inc(Name, TimeModule:merge(MsgTime, Time)),
            Log ! {log, Name, NewTime, {received, Msg}},
            loop(Name, Log, Peers, NewTime, Sleep, Jitter, TimeModule);
        
        stop ->
            ok;

        Error ->
            Log ! {log, Name, Time, {error, Error}}
    
    after Wait ->
        Selected = select(Peers),
        NewTime = TimeModule:inc(Name, Time),
        Message = {"hello! this is a test message.", rand:uniform(100)},
        Selected ! {msg, NewTime, Message},
        jitter(Jitter),
        Log ! {log, Name, NewTime, {sending, Message}},
        loop(Name, Log, Peers, NewTime, Sleep, Jitter, TimeModule)
    end.

select(Peers) ->
    lists:nth(rand:uniform(length(Peers)), Peers).

jitter(0) -> ok;
jitter(Jitter) -> timer:sleep(rand:uniform(Jitter)).