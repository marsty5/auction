-module(agent).

%% Exported Functions
-export([start/0, process_requests/2]).

%% Define an initialization value for the item
-define(initVal,100).

%% API Functions
start() ->
	AgentPid = spawn(agent, process_requests, [[], ?initVal]),
	io:format("~s ~p", ["Auction starts! Initial value:", ?initVal]),
	register(agent_process, AgentPid).
	
process_requests(Bidders, Bid) ->
	receive
		%% Receive JOIN request from a Bidder
		{bidder_join, Name, From} ->
			% Add Bidder into the list and broadcast the new list
			NewBidders = [{From,Name,0}|Bidders],	
			broadcast(NewBidders, {join, Name, Bid}),
			process_requests(NewBidders, Bid);

		%% Receive LEAVE request from a Bidder
		{bidder_leave, Name, From} ->
		io:format("Bidder ~w LEAVES.~n", Name),
			% Delete Bidder from the list and broadcast the new list to rest of bidders
			NewBidders = lists:keydelete(From, 1, Bidders), 
			if	%% If new list is empty, send a message to the node leaving  
				NewBidders==[]->
					Bid2=?initVal,
					broadcast(Bidders, {leave, Name,Bid2});
				true -> %% If new list not empty, change its structure in order to find the max bid in the list
					ModBidders=changeStrc(NewBidders,[]),
					%% Bid2 has the max bid of the list of peers
					{Bid2,[User,_]}=lists:max(ModBidders),
					if 	%% If Bid2 is less than initial value, it broadcasts the initial value 
						Bid2 < ?initVal ->
							broadcast(Bidders, {leave, Name, ?initVal});
						true -> %% Else, it broadcasts the Bid2 and the bidder doing this bid
							broadcast(Bidders, {leave, Name, Bid2, User})
					end

			end,
			process_requests(NewBidders,Bid2);

		%% Receive BID message from a Bidder
		{send, Name, NewBid,From} ->
			%io:format("Bidder ~s bids ~p amount!~n", [Name, NewBid]),
			%% If received bid is higher than existing one, delete old bid of this Bidder
			%% and broadcast new bid of Bidder
			io:format("name: ~w, NewBid: ~w, Bid: ~w, From: ~w ~n",[Name,NewBid,Bid,From]),
			if	
				Bid>NewBid ->				
					NewBidders = lists:keydelete(Name, 2, Bidders),
					io:format("[~s] ~p ~n", ["NewBid > Bid. Delete bidder:", From]),
					broadcast([{From,Name,NewBid}|NewBidders], {message, Name, NewBid}),
					process_requests([{From,Name,NewBid}|NewBidders],NewBid);
			true -> %% else IGNORE it!
				process_requests(Bidders,Bid)
			end			
 			{send, Name, Amount,From} ->
			if
				Amount>Bid ->
					%io:format("bid[~p] amount~p ~n", [Bid, Amount]),				
					NewClients = lists:keydelete(Name,2, Clients),
					broadcast([{From,Name,Amount}|NewClients], {message, Name, Amount}),
					process_requests([{From,Name,Amount}|NewClients],Amount);
				true ->
					%broadcast(Clients, {message, Name, Amount}),
					process_requests(Clients,Bid)
			end		
	
	%%after 90000 ->
	%% After an amount of seconds, it comes the end of the auction.
	%% If there are no bidders, the auction restarts with half value as the initial bid.
	%% Otherwise,  
	after 60000 ->
		if 	%% If there are no bidders, restart auction with half value
			Bidders==[] ->
				io:format("[~s] ~p ~n", ["Timeout! No bidders! Restart with New Initial Value:", Bid/2]),
				broadcast(Bidders, {join, "INFO",Bid/2}),
				process_requests(Bidders,Bid/2);
			true ->		
			%% Else, if there are bidders but 
				ModBidders=changeStrc(Bidders,[]),
				{Bid2,[User,_]}=lists:max(ModBidders),
				if Bid2>0->
					io:format("Sold to ~s! Final price: ~p ~n", [User,Bid2]);
				true ->
					io:format("[~s] ~p ~n", ["Timeout! No high bids! Restart with New Initial Value: ", Bid/2]),
					broadcast(Bidders, {join, "Info",Bid/2}),
					process_requests(Bidders,Bid/2)
				end
		end
	end.

%% Local Functions
%% broadcast - send message to list of peers
broadcast(PeerList, Message) ->
	Fun = fun({Peer,_,_}) -> Peer ! Message end,
	lists:map(Fun, PeerList).

%% changeStrc - change structure of list with bidders
changeStrc([{Peer,Name,Value}|T], Tail) ->
	[{Value,[Name,Peer]}|changeStrc(T, Tail)];

changeStrc([], Tail) ->		%% End of recursion
	Tail.
