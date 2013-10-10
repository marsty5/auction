auction
=======

[Erlang] Implementation of a real-time concurrent system that simulates an English auction

This is a small exercise I had to do for the course "Concurrent and Distributed Programming" at Universitat Politecnica de Catalunya (UPC) during my participation in the European Master in Distributed Computing (EMDC).

Description:<br>
An agent opens an auction for an item by giving an initial opening bid,
which is the minimum price that someone can bid.
Then, interested buyers show their wish to buy the item by bidding to this item,
increamenting each time the amount of bid.

After a predefined period of time, a timeout occurs and the bidder with the higher bid wins!
