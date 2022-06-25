-module(hist).
-export([new/1,update/3]).

%return a new history
%message from name will always be seen as old
new(Name)->
    [{Name,inf}].
    

%check the message number is new or old and then update the history
update(Node, N, History)->
   case lists:keyfind(Node, 1, History) of
	{Name, Len} ->
	    if 
		N > Len -> 
            NewHist = lists:keydelete(Name,1,History),

		    {new, [{Node, N}|NewHist]};
		true -> 
		    old
	    end;
	false ->
        NewHist2 = lists:keydelete(Node,1,History),
	    {new, [{Node, N}|NewHist2]}
    end.
