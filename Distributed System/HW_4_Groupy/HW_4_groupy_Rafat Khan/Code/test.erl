%% @author rafat
%% @doc @todo Add description to test.

-module(test).
-export([first/3,add/4,more/3,freeze/1,go/1,sleep/2,stop/1]).
-compile(export_all).

% First Worker
% W1 = test:first(1, gms1, 1000)

first(N, Module, Sleep) ->
   worker:start(N, Module, rand:uniform(256), Sleep).

% Additional workers
%  test:add(2, gms1, W1, 1000) 
%  test:add(3, gms1, W1, 1000)

add(N, Module, Wrk, Sleep) ->
   worker:start(N, Module, rand:uniform(256), Wrk, Sleep).

run1() ->
	Wrk = worker:start(1, gms1, rand:uniform(256), 1000),
	worker:start(2, gms1, rand:uniform(256), Wrk, 1000),
	worker:start(3, gms1, rand:uniform(256), Wrk, 1000),
	worker:start(4, gms1, rand:uniform(256), Wrk, 1000).

run2() ->
	Wrk2 = worker:start(1, gms2, rand:uniform(256), 1000),
	worker:start(2, gms2, rand:uniform(256), Wrk2, 1000),
	worker:start(3, gms2, rand:uniform(256), Wrk2, 1000),
	worker:start(4, gms2, rand:uniform(256), Wrk2, 1000).


run3() ->
	Wrk3 = worker:start(1, gms3, rand:uniform(256), 1000),
	worker:start(2, gms3, rand:uniform(256), Wrk3, 1000),
	worker:start(3, gms3, rand:uniform(256), Wrk3, 1000),
	worker:start(4, gms3, rand:uniform(256), Wrk3, 1000).


%% To create a number of workers in one go, 

more(N, Module, Sleep) when N > 1 ->
    Wrk = first(1, Module, Sleep),
    Ns = lists:seq(2,N),
    lists:map(fun(Id) -> add(Id, Module, Wrk, Sleep) end, Ns),
    Wrk.
		      

% These are messages that we can send to one of the workers. It will multicast it to all workers. They should (if everything works)
% They should receive the message at the same (logical) time.

freeze(Wrk) ->
    Wrk ! {send, freeze}.

go(Wrk) ->
    Wrk ! {send, go}.

sleep(Wrk, Sleep) ->
    Wrk ! {send, {sleep, Sleep}}.

stop(Wrk) ->
    Wrk ! {send, stop}.