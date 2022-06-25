%% @author rafat
%% @doc @todo Add description to test2.

-module(routy).
-export([start/2,stop/1,status/1]).

%start a new router process
start(Reg,Name)->
    register(Reg,spawn(fun()->init(Name) end)).

%stop the router process
stop(Node) -> 
    Node!stop,
    unregister(Node).

%intialize the router process
init(Name) ->
    Intf = intf:new(),
    Map = map:new(),
    Table = dijkstra:table(Intf,Map),
    Hist = hist:new(Name),
    router(Name,0,Hist,Intf,Table,Map).

%print status of node
status(Ref)->
    Ref ! {status,self()},
    receive
        {status,{Name,N,Hist,Intf,Table,Map}}->
            io:format("~w status:~n",[Ref]),
            io:format("Name: ~w~n",[Name]),
             io:format("N: ~w~n",[N]),
             io:format("History: ~w~n",[Hist]),
             io:format("Interfaces: ~w~n",[Intf]),
             io:format("Table: ~w~n",[Table]),
             io:format("Map: ~w~n",[Map]),
            ok
        after 
                10000->
                    io:format("fail to get status~n")
        end.


router(Name,N,Hist,Intf,Table,Map)->
    receive
        %receive message as destination
        {route,Name,_From,Message}->
            io:format("~w: received message ~w ~n",[Name,Message]),
            router(Name,N,Hist,Intf,Table,Map);

        %route message to the address
        {route,To,From,Message} ->
            io:format("~w: routing message: (~w) ~n",[Name,Message]),
            %find the desired gateway
            case dijkstra:route(To,Table) of
                {ok,Gw}->
                    %find pid of the process of desired gateway
                    case intf:lookup(Gw,Intf) of
                        %send message
                        {ok,Pid} ->
                            Pid ! {route,To,From,Message};
                        notfound ->
                            io:format("error: PID not found~n")
                    end;
                notfound->
                io:format("table not found error~n")
            end,
            router(Name,N,Hist,Intf,Table,Map);

        %send message to destination
        {send,To,Message}->
            self()!{route,To,Name,Message},
            router(Name,N,Hist,Intf,Table,Map);

        %update the link
        {links,Node,R,Links} ->
            %check the message is new or old
            case hist:update(Node,R,Hist) of
                {new,Hist1} ->
                    %send message to all interfaces
                    intf:broadcast({links,Node,R,Links},Intf),
                    %update the map link
                    Map1 = map:update(Node,Links,Map),
                    router(Name,N,Hist1,Intf,Table,Map1);
                %if the message is old, then do nothing
                old->
                    router(Name,N,Hist,Intf,Table,Map)
                end;

        % add an interface
        {add,Node,Pid}->
            Ref = erlang:monitor(process,Pid),
            Intf1 = intf:add(Node,Ref,Pid,Intf),
            router(Name,N,Hist,Intf1,Table,Map);

        %remove a linked node
        {remove,Node}->
            {ok,Ref} = intf:ref(Node,Intf),
            erlang:demonitor(Ref),
            Intf1 = intf:remove(Node,Intf),
            router(Name,N,Hist,Intf1,Table,Map);
        
        %get down message and remove the interface
        {'DOWN',Ref,process,_,_}->
            {ok,Down} = intf:name(Ref,Intf),
            io:format("~w: exit received from ~w~n",[Name,Down]),
            Intf1=intf:remove(Down,Intf),
            router(Name,N,Hist,Intf1,Table,Map);
        
        %return current status
        {status,From}->
            From!{status,{Name,N,Hist,Intf,Table,Map}},
            router(Name,N,Hist,Intf,Table,Map);

        %update the table
        update->
            Table1 = dijkstra:table(intf:list(Intf),Map),
            router(Name,N,Hist,Intf,Table1,Map);
        
        
        %broadcast the message
        %N+1 is the update the message counter
        broadcast->
            Message = {links,Name,N,intf:list(Intf)},
            intf:broadcast(Message,Intf),
            router(Name,N+1,Hist,Intf,Table,Map);
        
        %stop the router
        stop->
            ok
        end.