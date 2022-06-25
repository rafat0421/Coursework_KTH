/**
* Name: Auction
* Based on the internal empty template. 
* Author: rafat
* Tags: 
*/


model Auction

global 
{
	int guestNumber <- rnd(20)+20;
	float guestSpeed <- 0.5;
	point auctionerMasterLocation <- {0,50};
	list<string> items <- ["Cloths","Mobile","Laptop", "Gedgets"];
	// min and max for guest
	int priceMin <- 100;
	int priceMax <- 1500;
	// English auction configs
	int priceInecreaseDecreaseMin <- 10;
	int priceInecreaseDecreaseMax <- 100;
	// The initial price of the item to sell
	int auctionerEngPriceMin <- 0;
	int auctionerEngPriceMax <-1500;
	// Dutch auction configs 
	// initial price of the item to sell
	int auctionerDutchPriceMin <- 1200;
	int auctionerDutchPriceMax <-2000;
	// minimum price of the item, if the bids go below this the auction fails
	int auctionerMinimumValueMin <- 90;
	int auctionerMinimumValueMax <- 300;
	//type of auctions
	list<string> auctionTypes <- ["Dutch", "English", "Sealed"];
	
	init
	{
		create Guest number: guestNumber
		{
			// each guest prefers a random item
			preferredItem <- items[rnd(length(items) - 1)];
		}		
		create AuctionerMaster number: 1
		{
			location <- auctionerMasterLocation;
		}
	}
	
}

species Guest skills:[moving, fipa]
{
		bool wonAuction <- false;
	
	// Default color of guests
	rgb color <- #red;
	
	// This is the price at which the guest will buy merch, set in the configs above
	int guestMaxAcceptedPrice <- rnd(priceMin,priceMax);
	
	// Target to move towards
	point target <- nil;

	// Which auction is guest participating in
	Auctioner targetAuction;
	
	// each guest prefers a single piece of item
	string preferredItem;
	
	aspect default
	{
		draw sphere(2) at: location color: color;

		if (wonAuction = true)
		{
			if(preferredItem = "Cloths")
			{
				draw cube(1.2) at: location + point([2.1, 0.0, 2.0]) color: #purple;
			}
			else if(preferredItem = "Mobile")
			{
				draw pyramid(1.2) at: location + point([0.0, 0.0, 3.5]) color: #orange;
			}
			else if(preferredItem = "Laptop")
			{
				draw cylinder(2.01, 1.5) at: location + point([0.0, 0.0, 1.0]) color: #lime;
			}
			else if(preferredItem = "Gedgets")
			{
				draw cylinder(2.01, 1.5) at: location color: #pink;
			}
		}
	}
	
	//target auction is an auction selling the types of items the guest is interested in
	reflex inAuction when: targetAuction != nil
	{
		if(location distance_to(targetAuction.location) > 9)
		{
			target <- targetAuction;
		}
		else
		{
			target <- nil;
		}
	}
	
	reflex beIdle when: target = nil
	{
		do wander;
	}
	
	reflex moveToTarget when: target != nil
	{
		do goto target:target.location speed: guestSpeed;
	}
	
	reflex listen_messages when: (!empty(cfps))
	{
		message requestFromInitiator <- (cfps at 0);
		list<string> tlist <- requestFromInitiator.contents;
		// the request's format is as follows: [Status, Item, AuctionType, ...]
		if(tlist[0] = 'Start' and tlist[1] = preferredItem)
		{
			// If the guest receives a message from an auction selling its preferredItem,
			// the guest participates in that auction
			targetAuction <- requestFromInitiator.sender;

			// Send a message to the auctioner telling them the guest will participate
			write name + " joins " + requestFromInitiator.sender + "'s auction for " + preferredItem;
			// Essentially add the guest to the interestedGuests list
			targetAuction.interestedGuests <+ self;
		}
		//End of auction
		else if(tlist[0] = 'Stop')
		{
			write name + ' knows the auction is over.';
			targetAuction <- nil;
			target <- nil;
		}
		//Time to send bid for sealed bidding
		else if(tlist[0] = 'Bid For Sealed')
		{
			do start_conversation (to: requestFromInitiator.sender, protocol: 'fipa-propose', performative: 'propose', contents: ['This is my offer', guestMaxAcceptedPrice]);
			targetAuction <- nil;
			target <- nil;
		}
		//next round for english bidding
		else if(tlist[0] = 'Bid for English')
		{
			int currentBid <- int(tlist[1]);
			//can bid more
			if (guestMaxAcceptedPrice > currentBid) 
			{
				int newBid <- currentBid + rnd(priceInecreaseDecreaseMin, priceInecreaseDecreaseMax);
				if(newBid > guestMaxAcceptedPrice)
				{
					newBid <- guestMaxAcceptedPrice;
				}
				//write name + ' sending propose ' + newBid;
				do start_conversation (to: requestFromInitiator.sender, protocol: 'fipa-propose', performative: 'propose', contents: ['This is my offer', newBid]);
			}
			//can't bid more
			else
			{
				write name + ": Too much for me, I'm out!";
				do reject_proposal (message: requestFromInitiator, contents: ["Too much for me, I'm out guyzz"]);
				targetAuction <- nil;
				target <- nil;
			}
		}
		else if(tlist[0] = 'Winner')
		{
			wonAuction <- true;
			write name + ' won the auction for ' + preferredItem;
			if(preferredItem = "posh pants")
			{
				write "Go Pants !!!";
			}
		}
	}
	
	/*
	 * In Dutch auction, the auctioner proposes and the participant can accept or reject it, based on the price it would pay for it.
	 */
	reflex reply_messages when: (!empty(proposes))
	{
		message requestFromInitiator <- (proposes at 0);
		list<string> rfi <- requestFromInitiator.contents;
		// TODO: maybe define message contents somewhere, rn this works
		string auctionType <- rfi[1];
		if(auctionType = "Dutch")
		{
			int offer <- int(rfi[2]);
			if (guestMaxAcceptedPrice >= offer) {
				do accept_proposal with: (message: requestFromInitiator, contents: ["I, " + name + ", accept your offer of " + offer + ", merchant."]);
			}
			else
			{
				do reject_proposal (message: requestFromInitiator, contents: ["I, " + name + ", not interested!"]);	
				targetAuction <- nil;
				target <- nil;
			}
		}
	}
	
}// Guest end

//the AuctionerMaster creates auctioners
species AuctionerMaster skills:[fipa]
{
	bool auctionersCreated <- false;
	rgb myColor <- rnd_color(255);
	int mySize <- 10;
	bool auctionersInPosition <- false;
	list<Auctioner> auctioners <- [];
	
	aspect
	{
		draw pyramid(mySize) color: myColor;
	}
	
	reflex casinoLigths
	{
		myColor <- rnd_color(255);
		if(flip(0.5) and mySize < 15)
		{
			mySize <- mySize + 1;
		}
		else if(mySize >= 10)
		{
			mySize <- mySize - 1;
		}
	}
	
	//creates the auctioners within the random time range
	reflex createAuctioners when: !auctionersCreated and time rnd(0, 600)
	{
		string auctionersText <- name + " creating auctions: ";
		
		loop i from: 0 to: length(items)-1
		{
			create Auctioner
			{
				location <- myself.location;
				soldItem <- items[i];
				auctionersText <- auctionersText + name + " with " + items[i] + " ";
				targetLocation <- {rnd(100),rnd(100)};
				myself.auctioners <+ self;
			}
		}
		write auctionersText;
		auctionersCreated <- true;
	}	
} // AuctionerMaster ends

species Auctioner skills:[fipa, moving]
{
	// Auction's initial size and color, location used in the beginning
	int mySize <- 5;
	rgb myColor <- #gray;
	point targetLocation <- nil;
	// price of item to sell
	int auctionerDutchPrice <- rnd(auctionerDutchPriceMin, auctionerDutchPriceMax);
	int auctionerEngPrice <- rnd(auctionerEngPriceMin, auctionerEngPriceMax);
	// minimum price of item to sell. if max bid is lower than this, bid is unsuccessful
	int auctionerMinimumValue <- rnd(auctionerMinimumValueMin, auctionerMinimumValueMax);
	bool auctionRunning <- false;
	bool startAnnounced <- false;
	string auctionType <- auctionTypes[rnd(length(auctionTypes) - 1)];
	int currentBid <- 0;
	string currentWinner <- nil;
	message winner <- nil;
	string soldItem <- "";
	list<Guest> interestedGuests;

	aspect
	{
		draw pyramid(mySize) color: myColor;
	}
	
	//flashing and changing size for attraxting guests
	reflex casinoLigths when: targetLocation = nil
	{
		myColor <- rnd_color(255);
		if(flip(0.5) and mySize < 11)
		{
			mySize <- mySize + 1;
		}
		else if(mySize >= 8)
		{
			mySize <- mySize - 1;
		}
	}
	
	//go to auction location
	reflex goToLocation when: targetLocation != nil
	{
	 	if(location distance_to targetLocation <= 0.1)
	 	{
	 		write name + " has reached " + targetLocation;
	 		targetLocation <- nil;
	 	}
	 	else
	 	{
	 		do goto target: targetLocation speed: 5;	
	 		myColor <- #gray;
	 		mySize <- 5;
	 	}
	}
	
	//send out the first auction message to all guest after a random amount of time
	reflex sendMessageForStartingAuction when: !auctionRunning and time >= rnd(0,50) and targetLocation = nil and !startAnnounced
	{
		write name + " starting " + auctionType + " soon";
		do start_conversation (to: list(Guest), protocol: 'fipa-propose', performative: 'cfp', contents: ['Start', soldItem]);
		startAnnounced <- true;
	}
	
	//sets auction status to true when interested guests are nearby.
	reflex guestsAreAround when: !auctionRunning and !empty(interestedGuests) and (interestedGuests max_of (location distance_to(each.location))) <= 20
	{
		write name + " guestsAreAround";
		auctionRunning <- true;
	}

	//starting auctions
	reflex receiveAcceptMessages when: auctionRunning and !empty(accept_proposals)
	{
		if(auctionType = "Dutch") //auctioner sends a propose message and guests can reply with accept or reject messages. The auction ends with the first accept.
		{
			write name + ' receives accept messages';
			
			loop a over: accept_proposals {
				write name + ' got accepted by ' + a.sender + ': ' + a.contents;
				do start_conversation (to: a.sender, protocol: 'fipa-propose', performative: 'cfp', contents: ['Winner']);
			}
			targetLocation <- auctionerMasterLocation;
			auctionRunning <- false;
			//end of auction
			//iterate untill the end of auction
			do start_conversation (to: interestedGuests, protocol: 'fipa-propose', performative: 'cfp', contents: ['Stop']);
			interestedGuests <- [];
		}
	}

	reflex getProposes when: (!empty(proposes))
	{
		if(auctionType = "Sealed") //the participants send proposes to the auctioner. The highest bid wins right away.
		{
			targetLocation <- auctionerMasterLocation;
			auctionRunning <- false;

			loop p over: proposes {
				list<string> plist <- p.contents;
				write name + ' got an offer from ' + p.sender + ' of ' + plist[1] + ' kr.';
				if(currentBid < int(plist[1]))
				{
					currentBid <- int(plist[1]);
					currentWinner <- p.sender;
					winner <- p;
				}
			}
			do start_conversation (to: winner.sender, protocol: 'fipa-propose', performative: 'cfp', contents: ['Winner']);
			write name + ' bid ended. Sold to ' + currentWinner + ' for: ' + currentBid;
			do accept_proposal with: (message: winner, contents: ['Item is yours']);
			do start_conversation (to: interestedGuests, protocol: 'fipa-propose', performative: 'cfp', contents: ["Stop"]);
			interestedGuests <- [];
		}
		else if(auctionType = "English") //the participants send proposes to the auctioner which sets the current highest bid and the auction goes on.
		{
			loop p over: proposes {
				list<string> plist <- p.contents;
				write name + ' got an offer from ' + p.sender + ' of ' + plist[1] + ' kr.';
				if(currentBid < int(plist[1]))
				{
					currentBid <- int(plist[1]);
					currentWinner <- p.sender;
					winner <- p;
				}
			}
		}
	}
	
	//reject messages are used in Dutch and English auctions.
	reflex receiveRejectMessages when: auctionRunning and !empty(reject_proposals)
	{
		if(auctionType = "Dutch") //start from high bid which keep decreasing in each round as long as everybody rejects the proposal
		{
			write name + ' receives reject messages';
			
			auctionerDutchPrice <- auctionerDutchPrice - rnd(priceInecreaseDecreaseMin, priceInecreaseDecreaseMax);
			if(auctionerDutchPrice < auctionerMinimumValue)
			{
				targetLocation <- auctionerMasterLocation;
				auctionRunning <- false;

				write name + ' price went below minimum value (' + auctionerMinimumValue + '). No more auction for thrifty guests!';
				do start_conversation (to: interestedGuests, protocol: 'fipa-propose', performative: 'cfp', contents: ['Stop']);
				interestedGuests <- [];
			}
		}
		else if(auctionType = "English") //means that participants don't wish to bid more and are out of the auction.
		{	
			loop r over: reject_proposals 
			{
				interestedGuests >- r.sender;
			}
			if(length(interestedGuests) < 2)
			{
				targetLocation <- auctionerMasterLocation;
				auctionRunning <- false;

				if(currentBid < auctionerMinimumValue)
				{
					write name + ' bid ended.';
				}
				else
				{
					write 'Bid ended. Winner is: ' + currentWinner + ' with a bid of ' + currentBid;	
					do start_conversation (to: winner.sender, protocol: 'fipa-propose', performative: 'cfp', contents: ['Winner']);
				}
				if(!empty(interestedGuests))
				{
					do start_conversation (to: interestedGuests, protocol: 'fipa-propose', performative: 'cfp', contents: ["Stop"]);
				}
				interestedGuests <- [];
			}
		}
	}
	
	reflex sendAuctionInfo when: auctionRunning and time >= 50 and !empty(interestedGuests){
		if(auctionType = "Dutch") //every iteration, it sends the decreased price of the item to the participants which they can accept of reject
		{
			write name + ' sends the offer of ' + auctionerDutchPrice +' kr to participants';
			do start_conversation (to: interestedGuests, protocol: 'fipa-propose', performative: 'propose', contents: ['Bid for Dutch', auctionType, auctionerDutchPrice]);
		}
		else if(auctionType = "English") //every iteration, tells guests about the current highest bid that they need to outbid
		{
			write 'Auctioner ' + name + '(English Auction): current bid is: ' + currentBid + '.';
			do start_conversation (to: interestedGuests, protocol: 'fipa-propose', performative: 'cfp', contents: ["Bid for English", currentBid]);
		}
		else if(auctionType = "Sealed") //only one iteration
		{
			write 'Auctioner ' + name + '(Sealed Auction): place your bid.';
			do start_conversation (to: interestedGuests, protocol: 'fipa-propose', performative: 'cfp', contents: ['Bid For Sealed']);
		}
	}	
} //Auctioner ends

experiment main type: gui
{	
	output
	{
		display map type: opengl
		{
			species Guest;
			species AuctionerMaster;
			species Auctioner;
		}
	}
}

