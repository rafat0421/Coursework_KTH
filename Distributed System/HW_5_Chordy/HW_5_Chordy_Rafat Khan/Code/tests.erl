%% @author rafat
%% @doc @todo Add description to tests.

-module(tests).
-export([compile/0,unit/0,run1/0,run2/0, testSingle/1, testList/2]).

compile() ->
    % Compile the sources.
    SOURCES = [key,node1,run1,node2,run2,storage],
    lists:foreach(fun(X)-> case cover:compile(X) of {error,_} -> error("Compilation error"); _ -> ok end end, SOURCES),
    ok.

unit() ->
    compile(),
    % Key
    %test("key:between 20 in 10,30 ?",key:between(20,10,30)==true),
    %test("key:between 20 not in 30,10 ?",key:between(20,30,10)==false),
    %test("key:between 10 not in 20,30 ?",key:between(10,20,30)==false),
    %test("key:between 10 in 30,20 ?",key:between(10,30,20)==true),
    %test("key:between 10 not in 10,20 ?",key:between(10,10,20)==false),
    %test("key:between 30 in 10,10 ?",key:between(30,10,10)==true),

    %test("key:between 10 not in 10,20 ?",key:between(10,10,20)==false),
    %test("key:between 20 not in 10,20 ?",key:between(20,10,20)==false),
    %test("key:between 10 not in 20,10 ?",key:between(10,20,10)==false),
    %test("key:between 20 not in 20,10 ?",key:between(20,20,10)==false),

    ok.

run1() ->
    unit(),
    run1:start().

run2() ->
    % check compilation & unit-tests for the key module
    unit(),
    % start and setup the ring
    Nodes = run2:start(5),
    % select a node as our entry point for the rest of the tests
    [Node |_] = Nodes,
    {_,Pid} = Node,
    % wait for init because of bad handling of nil (different format than ther addresses...)

	%timer:sleep(5000),
	%test_one_item([Pid]),
	%timer:sleep(5000),
	%Pid ! probe,
	%timer:sleep(5000),
	%test_a_list_of_items(10000,[Pid]),
	%timer:sleep(5000),
	%node2:start(key:generate(),Pid,[]),
    
	timer:apply_after(6000, tests, testSingle, [Pid]),
    timer:send_after(7000, Pid, probe),
    timer:apply_after(8000, tests, testList, [10000,Pid]),
    timer:apply_after(8050, node2, start, [key:generate(),Pid,[]]),
    
    %timer:send_after(12000, Pid, probe),
    Nodes.

testSingle(Npid) ->
    addEntry(testKey,testValue, Npid),
    Entry = getEntry(testKey, Npid),
    test("test single item : ",Entry=={testKey,testValue}).

testList(N,Npid) ->
    % Generate a list of random number
    Keys = [random:uniform(1000000) || _ <- lists:seq(1, N)],
    % Add entries of the form {N,N}.
    lists:foreach( fun(I) -> addEntry(I,I,Npid) end, Keys),
    % Lookup the value for every key.
    Values = lists:map( fun(I) -> {_,V} = getEntry(I,Npid), V end, Keys ),
    % Assert.
    test("test a list of items : ",Keys==Values),
    ok.

addEntry(Key, Value, Npid) ->
    Qref = node2:addEntry(Key,Value, Npid, self()),
    receive
        {Qref, ok} -> ok
    after 2000 -> io:format("ADD (~p) Time out...~n",[{Key,Value}])
    end.


getEntry(Key, Npid) ->
    Qref = node2:getEntry(Key, Npid, self()),
    receive
        {Qref, Result} -> Result
    after 2000 -> io:format("LOOKUP (~p) Time out...~n",[{Key}])
    end.

test(Label,Expr)->
    case Expr of
        true ->
            io:format("~p : completed successfully ~n",[Label]);
        false ->
            io:format("~n                    ~p : FAIL~n~n",[Label])
    end.