%% @author rafat
%% @doc @todo Add description to server.


-module(server).

%% ====================================================================
%% API functions
%% ====================================================================

-export([start/1 , start/2, stop/0]).
start(Port) ->
    start(Port, 1).

start(Port, N) ->	
	register(server, spawn(fun() -> init(Port, N) end)).
		
stop() ->
	server ! stop,
	ok.
	
%% ====================================================================
%% Internal functions
%% ====================================================================

%init function with parameter N --> Number of handlers to create
init(Port,N) ->
	Opt = [list, {active, false}, {reuseaddr, true}],
	case gen_tcp:listen(Port, Opt) of
		{ok, Listen} ->			
			handlers(Listen,N),			
			wait();
		{error, Error} ->
			error
	end.

wait()->
	receive
		stop -> 
			ok
	end.

handlers(Listen,N)->
	case N of
	0 ->
	    ok;
	N ->
	    spawn(fun() -> handler(Listen,N) end),
	    handlers(Listen, N-1)
    end.


handler(Listen,I) ->
	case gen_tcp:accept(Listen) of
		{ok, Client} ->
			%io:format("rudy ~w: received request~n", [I]),
 			request(Client),
			handler(Listen,I);
		{error, Error} ->
			error
	end.

request(Client) ->
	Recv = gen_tcp:recv(Client, 0),
	case Recv of 
		{ok, Str} ->
			Request = http:parse_request(Str),
			Response = reply(Request),
			gen_tcp:send(Client, Response);			
		{error, Error} ->
			io:format("rudy: error: ~w~n", [Error])
	end,
	gen_tcp:close(Client).


reply({{get, URI, _}, _, _}) ->
    timer:sleep(40),
    http:ok(URI).

reply2({{get, URI, _}, _, _}) ->		
	%timer:sleep(40),
	[$/|FileName] = URI,
	case file:open(FileName, read) of
		{ok, Binary} -> 
			Data = readData(Binary),						
			file:close(Binary),			
			http:ok([Data]);
		{error, Reason} ->
			io:format("Error case file: ~s~n", [FileName]),
			Msg = "Get request received for non-existing file: " ++ FileName, 			
 			http:ok([Msg])		
	
%	B = iolist_to_binary("Rafat"),
%    iolist_to_binary(
%      io_lib:fwrite(
%         "HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
%         [size(B), B])).
	
	end.

response(Str) ->
    B = iolist_to_binary(Str),
    iolist_to_binary(
      io_lib:fwrite(
         "HTTP/1.0 200 OK\nContent-Type: text/html\nContent-Length: ~p\n\n~s",
         [size(B), B])).

readData(Device) ->
	case file:read_line(Device) of
		eof -> [];
		{ok, Data} -> [Data | readData(Device)]		
	end.