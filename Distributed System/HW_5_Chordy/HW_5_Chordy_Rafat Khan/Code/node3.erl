%% @author rafat
%% @doc @todo Add description to node3.


%detect failure
-module(node3).
-export([node/5,start/1,start/3, addEntry/4, getEntry/3]).
-define(Stabilize,1000).
-define(Timeout,1000).

%we are the first node
start(Id) ->
    start(Id, nil,[]).

start(Id, Peer, Store) ->
    spawn(fun() -> init(Id, Peer, Store) end).

%init function
%to stabilize the node
init(Id, Peer, Store) ->
    Predecessor = nil,
    {ok,Successor} = connect(Id,Peer),
    schedule_stabilize(),
    node(Id,Predecessor,Successor,nil,storage:create()).

%myself is the successor
connect(Id,nil)->
    %no reference
    {ok,{Id,nil,self()}};
%find the successor
connect(_Id,Peer)->
    Qref = make_ref(),
    %ask peer for the key
    Peer!{key,Qref,self()},
    receive
        %get the key
        {Qref,Skey}->
            %monitor peer
            Ref = monitor(Peer),
            {ok,{Skey,Ref,Peer}}
        after ?Timeout->
            io:format("Time out:no response~n",[])
        end.



%node function for listening
node(Id, Predecessor,Successor,Next,Store)->
    receive
        %a peer needs to know the key
        {key, Qref, Peer}->
            Peer ! {Qref, Id},
            node(Id, Predecessor, Successor,Next,Store);
        %a new node informs us the existence
        {notify, New}->
            {Pred,S} = notify(New, Id, Predecessor,Store),
            node(Id, Pred, Successor,Next,S);
        %a predecessor needs to know our predecessor
        {request, Peer}->
            
            request(Peer,Predecessor,Successor),
            node(Id,Predecessor,Successor,Next,Store);
        %our successor informs us about its predecessor
        {status,Pred,Nx}->
            {Succ,Nxt} = stabilize(Pred,Nx, Id, Successor),
            node(Id, Predecessor,Succ,Nxt,Store);
        %create a probe
        probe->
            create_probe(Id,Successor),
            node(Id,Predecessor,Successor,Next,Store);
        %we know that we send the probe since Id is equal
        {probe,Id,Nodes,T}->
            remove_probe(T,Nodes),
            node(Id,Predecessor,Successor,Next,Store);
        %pass the probe to next node
        {probe,Ref,Nodes,T}->
            forward_probe(Ref,T,Nodes,Id,Successor),
            node(Id,Predecessor,Successor,Next,Store);
        %add a key value pair
        {add,Key,Value,Qref,Client}->
            Added = add(Key,Value,Qref,Client,Id,Predecessor,Successor,Store),
            node(Id,Predecessor,Successor,Next,Added);
        %lookup for key value pair
        {lookup,Key,Qref,Client}->
            lookup(Key,Qref,Client,Id,Predecessor,Successor,Store),
            node(Id,Predecessor,Successor,Next,Store);
        %new nodes want to join
        %update the storage
        {handover,Elements}->
            Merged = storage:merge(Store,Elements),
            node(Id,Predecessor,Successor,Next,Merged);

           %the moniter finds the process is down 
        {'DOWN',Ref,process,_,_}->
            {Pred, Succ, Nxt} = down(Ref, Predecessor, Successor, Next),
            node(Id, Pred, Succ, Nxt, Store);

        %to stabilize
        %use when a new node is added
        stabilize->
            stabilize(Successor),
            node(Id,Predecessor,Successor,Next,Store)
        end.


%when the predecessor dies
down(Ref, {_, Ref, _}, Successor, Next) ->
            io:format("predecessor die:~w~n",[Ref]),
            {nil,Successor,Next};
%when the successor dies
%next becomes the successor
down(Ref, Predecessor, {_, Ref, _}, {Nkey,_, Npid}) ->
            io:format("successor die:~w~n",[Ref]),  
            self()!stabilize,
            %the next node becomes the successor
            Nref = monitor(Npid),
            {Predecessor, {Nkey, Nref, Npid}, nil}.


%send a request message to myself
%request for stabilizing
stabilize({_,_,Spid})->
    Spid!{request,self()}.


%Pred is the successor's current predecessor
stabilize(Pred,Next, Id, Successor)->
    {Skey,Sref,Spid} = Successor,
    case Pred of
        %the successor do not know about us
        %notify it about us
        nil ->
            Spid!{notify,{Id,self()}},
            {Successor,Next};
        %point to ouselves
        %do nothing
        {Id,_}->
            {Successor,Next};
        %point to itself
        %notify it about us
        {Skey,_}->
             Spid!{notify,{Id,self()}},
             {Successor,Next};
        %point to another node
        {Xkey,Xpid}->
            case key:between(Xkey,Id,Skey) of
                %we are before the predecessor of the target node
                %adopt the successor and run this function again
                true->
                    Xpid ! {notify, {Id, self()}},
                    drop(Sref),

                    
                    %monitor the predecessor as the successor
                    XNref = monitor(Xpid),
                    %Xpid ! {request, self()},
                    self() ! stabilize,
                    %stabilize(Pred, Next, Id, {Xkey,Xref,Xpid}),
                    %predecessor is the successor
                    %successor is the next
                    {{Xkey,XNref,Xpid},Successor};
                %we are the predecessor of the target node than its original predecessor
                false->
                    Spid!{notify,{Id,self()}},
                    {Successor,Next}
                end
            end.

%set a time period
schedule_stabilize()->
    timer:send_interval(?Stabilize,self(),stabilize).


%Peer wants to know our predecessor
request(Peer,Predecessor,Next)->
    case Predecessor of
        nil->
            %send to peer that out predecessor is nil 
            Peer ! {status,nil,Next};
        {Pkey,_,Ppid}->
            %send to peer our predecessor
            Peer!{status,{Pkey,Ppid},Next}
        end.

%get the information of a possible new predecessor
%need to check by myselfs
notify({Nkey,Npid},Id,Predecessor,Store)->
    case Predecessor of
        nil->
            %update the storage
            Keep = handover(Id,Store,Nkey,Npid),
            Nref = monitor(Npid),
            %return new pred and storage
            {{Nkey,Nref,Npid},Keep};
        {Pkey,Pref,_}->
            case key:between(Nkey,Pkey,Id) of
                true->
                    %update the storage
                    Keep = handover(Id,Store,Nkey,Npid),
                    drop(Pref),
                    Nref = monitor(Npid),
                    {{Nkey,Nref,Npid},Keep};
                %new node is before the predecessor
                false->
                    %Npid!{status, Predecessor},
                    {Predecessor,Store}
                end
            end.

%split the storage
%one part is kept, another part is passed to the new node
handover(Id, Store, Nkey, Npid) ->
    {Keep, Rest} = storage:split(Id, Nkey, Store),
    Npid ! {handover, Rest},
    Keep.


%create a probe with time stamp
create_probe(Id,Successor)->
    {_,_,Pid} = Successor,
    CurrentT = erlang:now(),
    %io:format("create T:~w~n",[CurrentT]),
    Pid!{probe,Id,[Id],CurrentT}.

%the probe which is created by us return to us
remove_probe(T,Nodes)->
    %timer:sleep(1000),
    ArrivedT = erlang:now(),
    %io:format("remove T:~w~n",[ArrivedT]),
    Diff = timer:now_diff(ArrivedT, T),
    io:format("The nodes are ~w.~n The time to pass around the ring is ~w.~n",[Nodes,Diff]).

%pass the probe to next node
forward_probe(Ref,T,Nodes,Id,Successor)->
    {_,_,Pid} = Successor,
    Pid!{probe,Ref,[Id|Nodes],T}.

%check if we should take care of the key (predecessor, us]
%if we should, then add the new key value
%if we should not, give the key to our successor
add(Key,Value,Qref,Client,Id,{Pkey,_},{_,Spid},Store)->
 case key:between(Key,Pkey,Id) of
     true->
         Client!{Qref,ok},
         storage:add(Key,Value,Store);
    false->
        Spid!{add,Key,Value,Qref,Client},
        Store
    end.

%check if we are responsible for the key
%if so, we lookup in the store and send the reply
%if not, we forward the request
lookup(Key,Qref,Client,Id,{Pkey,_},Successor,Store)->
    case key:between(Key,Pkey,Id) of
        true->
            Result = storage:lookup(Key,Store),
            Client!{Qref,Result};
        false->
            {_,Spid} = Successor,
            Spid!{lookup, Key,Qref,Client}
        end.

%monitor the process
monitor(Pid) ->
    erlang:monitor(process, Pid).

%drop the process monitor
drop(nil) ->
    ok;
drop(Pid) ->
    erlang:demonitor(Pid, [flush]).

addEntry(Key, Value, NodePid, Client) ->
    Qref = make_ref(),
    NodePid ! {add, Key, Value, Qref, Client},
    Qref.

getEntry(Key, NodePid, Client) ->
    Qref = make_ref(),
    NodePid ! {lookup, Key, Qref, Client},
	%io:format("LookUp (~p) successfully completed...~n",[{Key}]),
    Qref.
