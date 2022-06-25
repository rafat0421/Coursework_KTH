%% @author rafat
%% @doc @todo Add description to test3.

-module(test3).

-export([bench/3, sequential/3, parallel/3]).

bench(Host, Port, Num) ->
    io:format("Running ~w requests.~n", [Num]),
    SeqTime = sequential(Num, Host, Port),
    io:format("~w sequential requests took ~wms (~.2fms per request on avereage).~n", [Num, SeqTime, SeqTime / Num]),
    {ParTime, ErrNum} = parallel(Num, Host, Port),
    if
        ErrNum == 0 ->
            io:format("~w parallel requests took ~wms (~.2fms per request on avereage).~n", [Num, ParTime, ParTime / Num]);
        true ->
            io:format("~w parallel requests took ~wms, of which ~w failed (~.2fms per sucessful request).~n", [Num, ParTime, ErrNum, ParTime / (Num - ErrNum)])
    end.

%send request sequentially
sequential(N, Host, Port) ->
    Start = erlang:system_time(milli_seconds),
    run(N, Host, Port),
    Finish = erlang:system_time(milli_seconds),
    Finish - Start.

parallel(N, Host, Port) ->
    Start = erlang:system_time(milli_seconds),
    ErrorNo = parallelrun(N, Host, Port),
    Finish = erlang:system_time(milli_seconds),
    {Finish - Start, ErrorNo}.

parallelrun(N, Host, Port) ->
    parallellaunch(N, Host, Port),
    parallelwait(N, 0).

%create individual process for each request
parallellaunch(N, Host, Port) ->
    if
        N < 1 ->
            ok;
        true ->
			%io:format("launch"),
            Pid = self(),
            spawn(fun() -> Pid ! request(Host, Port) end),
            parallellaunch(N - 1, Host, Port)
    end.


parallelwait(N, EN) ->
    if
        N < 1 ->
            EN;
        true ->
            receive
                {error, _} ->
                    parallelwait(N - 1, EN + 1);
                ok ->
					%io:format("wait"),
                    parallelwait(N - 1, EN)
            end
    end.

run(N, Host, Port) ->
    if
        N < 1 ->
            ok;
        true ->
            request(Host, Port),
            run(N - 1, Host, Port)
    end.

request(Host, Port) ->
    Options = [list, {active, false}, {reuseaddr, true}],
    case gen_tcp:connect(Host, Port, Options) of
        {ok, Connection} ->
            gen_tcp:send(Connection, http:get("/foo.txt")),
            Recv = gen_tcp:recv(Connection, 0),
            gen_tcp:close(Connection),
            case Recv of
                {ok, _} ->
                    ok;
                {error, Error} ->
                    {error, Error}
            end;
        {error, Error} ->
            {error, Error}
    end.