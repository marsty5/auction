-module(bidder).

%% Exported Functions
-export([start/2,init_bidder/2,process_requests/0]).

%% API Functions
%% Create a bidder process
start(AgentPid, BidderName) ->
	BidderPid = spawn(bidder, init_bidder, [AgentPid, BidderName]),
	process_commands(AgentPid, BidderName, BidderPid).

%% Initialize bidder process - send to agent bidder's information
init_bidder(AgentPid, BidderName) ->
	AgentPid ! {bidder_join, BidderName, self()},
	process_requests().

%% Local Functions
process_requests() ->
	receive
		{join, Name, Bid} ->
			io:format("[JOIN] ~s joins the auction. Current Bid: ~p ~n", [Name,Bid]),
			process_requests(); 
		{leave, Name, Bid} ->
			%io:format("[LEAVE] ~s leaves the auction. Current Bid: ~p ~n", [Name,Bid]),
			io:format("[LEAVE] ~w leaves the auction. Current Bid: ~w ~n", [Name,Bid]),
			io:format("no one left with the bid so again initial amount ~p ~n", [Bid]),
			process_requests();
		{leave, Name, Bid, NewUser} ->
			io:format("[LEAVE] ~s leaves the auction. New highest Bidder: ~p. Bid: ~p ~n", [Name,NewUser,Bid]),
			process_requests();
		{message, Name, Text} ->
			io:format("[BID] ~s wants to update bid to ~w ~n", [Name, Text]),
			process_requests() 
	end.

process_commands(AgentPid, BidderName, BidderPid) ->
	%% Read from standard input the bid from user and send it to server
	{ok, Text} = io:fread("Bid to win! Your Bid: ", "~d"),
	if
		Text == -1 ->	%% If bid is -1, then the bidder leaves the auction.
			AgentPid ! {bidder_leave, BidderName, BidderPid};
		true ->
			AgentPid ! {send, BidderName, Text, BidderPid},
			process_commands(AgentPid, BidderName, BidderPid)
	end.
