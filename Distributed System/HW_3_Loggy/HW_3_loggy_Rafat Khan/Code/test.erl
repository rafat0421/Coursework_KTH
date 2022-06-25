%% @author rafat
%% @doc @todo Add description to test.

-module(test).

-export([run/0, run/3, run/4]).
-compile(export_all).

run() ->
    run(1000, 500, 10).

run(Time, Sleep, Jitter) ->
    io:format("Running Lamport time-stamp test:~n"),
    run(Time, Sleep, Jitter, time),
    timer:sleep(1000),
    io:format("~n~nRunning Vector time-stamp test:~n"),
    run(Time, Sleep, Jitter, mvect).

run(Time, Sleep, Jitter, TimeModule) ->
    io:format("Running test for ~w seconds:~n", [Time / 1000]),
    Log = log:start([worker1, worker2, worker3, worker4], TimeModule),
    A = worker:start(worker1, Log, 10, Sleep, Jitter, TimeModule),
    B = worker:start(worker2, Log, 20, Sleep, Jitter, TimeModule),
    C = worker:start(worker3, Log, 30, Sleep, Jitter, TimeModule),
    D = worker:start(worker4, Log, 40, Sleep, Jitter, TimeModule),
    worker:peers(A, [B, C, D]),
    worker:peers(B, [A, C, D]),
    worker:peers(C, [A, B, D]),
    worker:peers(D, [A, B, C]),
    timer:sleep(Time),
    io:format("Stopping test~n"),
    worker:stop(A),
    worker:stop(B),
    worker:stop(C),
    worker:stop(D),
    log:stop(Log).