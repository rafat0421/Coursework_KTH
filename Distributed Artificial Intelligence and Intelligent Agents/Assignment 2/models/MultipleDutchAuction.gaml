/**
* Name: MultipleDutchAuction
* Based on the internal empty template. 
* Author: rafat
* Tags: 
*/


model MultipleDutchAuction

/* Insert your model definition here */
global 
{
	
	int guestNumber <- rnd(20)+20;
	
	float guestSpeed <- 0.5;
	
	
	point auctionMasterLocation <- {-10,50};
	list<string> itemsAvailable <- ["Instruments","signed shirts","memorabillia", "posters and artwork"];
	
	// Time when auctioneers are created
	int auctionCreationMin <- 0;
	int auctionCreationMax <- 50;
	
	// Guest accepted price range min and max
	int guestAcceptedPriceMin <- 100;
	int guestAcceptedPriceMax <- 1500;
	
	// The initial price of the item to sell
	
	int dutchAuctionDecreaseMin <- 5;
	int dutchAuctionDecreaseMax <- 15;
	
	// The initial price of the item to sell, set above the max price so that no guest immediately wins
	int auctionerDutchPriceMin <- 1504;
	int auctionerDutchPriceMax <-1600;

	// Minimum price of the item, if the bids go below this the auction fails
	int minAuctionPrice <- 90;
	int maxAuctionPrice <- 300;
	
	list<string> auctionTypes <- ["Dutch"];

	
	
	init
	{
		/* Create guestNumber (defined above) amount of Guests */
		create Guest number: guestNumber
		{
			// Each guest prefers a random item
			preferredItem <- itemsAvailable[rnd(length(itemsAvailable) - 1)];
		}
		
		create AuctionMaster
		{
			
		}
	}
	
}

species Guest skills:[moving, fipa]
{
	// Default hunger vars
	
	bool wonAuction <- false;
	
	// Default color of guests
	rgb color <- #red;
	
	// This is the price at which the guest will buy merch, set in the configs above
	int guestMaxAcceptedPrice <- rnd(guestAcceptedPriceMin,guestAcceptedPriceMax);
	
	Auctioner targetAuction;
	Auctioner target;
	
	string preferredItem;
	
	aspect default
	{
		draw sphere(2) at: location color: color;

		if (wonAuction = true)
		{
			if(preferredItem = "instruments")
			{
				draw cube(1.2) at: location + point([2.1, 0.0, 2.0]) color: #purple;
			}
			else if(preferredItem = "signed shirts")
			{				
				draw pyramid(1.2) at: location + point([0.0, 0.0, 3.5]) color: #orange;
			}
			else if(preferredItem = "memorabillia")
			{
				draw cylinder(2.01, 1.5) at: location + point([0.0, 0.0, 1.0]) color: #lime;
			}
			else if(preferredItem = "posters and artwork")
			{
				draw cylinder(2.01, 1.5) at: location color: #pink;
			}
		}
	}
	
	
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
		write 'Test: ' + requestFromInitiator.contents;
		// the request's format is as follows: [String, auctionType, soldItem, ...]
		if(requestFromInitiator.contents = 'Start' and requestFromInitiator.contents = preferredItem)
		{
			// If the guest receives a message from an auction selling its preferredItem,
			// the guest participates in that auction
			targetAuction <- requestFromInitiator.sender;

			// Send a message to the auctioner telling them the guest will participate
			write name + " joins " + requestFromInitiator.sender + "'s auction for " + preferredItem;
			// TODO: handle this better
			// Essentially add the guest to the interestedGuests list
			targetAuction.interestedGuests <+ self;
		}
		//End of auction
		else if(requestFromInitiator.contents = 'Stop')
		{
//			
			write name + ' knows the auction is over.';
			targetAuction <- nil;
			target <- nil;
			
		}
		
		else if(requestFromInitiator.contents = 'Winner')
		{
			wonAuction <- true;
			write name + ' won the auction for ' + preferredItem;
			if(preferredItem = "posters and artwork")
			{
				write "Noiiiiice !!!";
			}
		}
	}
	
	
	reflex reply_messages when: (!empty(proposes))
	{
		message requestFromInitiator <- (proposes at 0);
		// TODO: maybe define message contents somewhere, rn this works
		string auctionType <- requestFromInitiator.contents;
		if(auctionType = "Dutch")
		{
			int offer <- int(requestFromInitiator.contents);
			if (guestMaxAcceptedPrice >= offer) {
				do accept_proposal with: (message: requestFromInitiator, contents: ["I, " + name + ", accept your offer of " + offer + ", merchant."]);
			}
			else
			{
				do reject_proposal (message: requestFromInitiator, contents: ["I, " + name + ", already have a house full of crap, you scoundrel!"]);	
				targetAuction <- nil;
				target <- nil;
			}
		}
	}
	
}
species AuctionMaster skills:[fipa]
{
	bool auctionersCreated <- false;
	list<Auctioner> auctioners <- [];

	reflex createAuctioners when: !auctionersCreated and time rnd(auctionCreationMin, auctionCreationMax)
	{
		string genesisString <- name + " creating auctions: ";
		
		loop i from: 0 to: length(itemsAvailable)-1
		{
			create Auctioner
			{	
				location <- {rnd(100),rnd(100)};
				soldItem <- itemsAvailable[i];
				genesisString <- genesisString + name + " with " + itemsAvailable[i] + " ";
				myself.auctioners <+ self;
			}
		}
		write genesisString;
		auctionersCreated <- true;
	}	
}

species Auctioner skills:[fipa, moving]
{
	// Auction's initial size and color, location used in the beginning
	int mySize <- 10;
	rgb myColor <- #blueviolet;
	point targetLocation <- nil;
	
	// price of item to sell
	int auctionerDutchPrice <- rnd(auctionerDutchPriceMin, auctionerDutchPriceMax);
	//int auctionerEngPrice <- rnd(auctionerEngPriceMin, auctionerEngPriceMax);
	// minimum price of item to sell. if max bid is lower than this, bid is unsuccessful
	int auctionerMinimumValue <- rnd(minAuctionPrice, maxAuctionPrice);
	
	// vars related to start and end of auction
	bool auctionRunning <- false;
	bool startAnnounced <- false;
	
	string auctionType <- auctionTypes[rnd(length(auctionTypes) - 1)];
	int currentBid <- 0;
	string currentWinner <- nil;
	message winner <- nil;

	// The kind of an item the merchant is selling
	string soldItem <- "";
	// The guests participating in the auction
	list<Guest> interestedGuests;

	aspect
	{
		
		draw circle(mySize) color: myColor;
		//draw pyramid(mySize) color: myColor;
	}
	
	

	reflex sendStartAuction when: !auctionRunning and time >= 90 and targetLocation = nil and !startAnnounced
	{
		write name + " starting " + auctionType + " soon";
		do start_conversation (to: list(Guest), protocol: 'fipa-propose', performative: 'cfp', contents: ['Start', soldItem]);
		startAnnounced <- true;
		
	}
	
	reflex guestsAreAround when: !auctionRunning and !empty(interestedGuests) and (interestedGuests max_of (location distance_to(each.location))) <= 13
	{
		write name + " guestsAreAround";
		auctionRunning <- true;
	}

	reflex receiveAcceptMessages when: auctionRunning and !empty(accept_proposals)
	{
		if(auctionType = "Dutch")
		{
			write name + ' receives accept messages';
			
			loop a over: accept_proposals {
				write name + ' got accepted by ' + a.sender + ': ' + a.contents;
				do start_conversation (to: a.sender, protocol: 'fipa-propose', performative: 'cfp', contents: ['Winner']);
			}
			targetLocation <- auctionMasterLocation;
			auctionRunning <- false;
			//end of auction
			do start_conversation (to: interestedGuests, protocol: 'fipa-propose', performative: 'cfp', contents: ['Stop']);
			interestedGuests <- [];
			do die;
		}
	}


	reflex receiveRejectMessages when: auctionRunning and !empty(reject_proposals)
	{
		if(auctionType = "Dutch")
		{
			write name + ' receives reject messages';
			
			auctionerDutchPrice <- auctionerDutchPrice - rnd(dutchAuctionDecreaseMin, dutchAuctionDecreaseMax);
			if(auctionerDutchPrice < auctionerMinimumValue)
			{
				targetLocation <- auctionMasterLocation;
				auctionRunning <- false;

				write name + ' price went below minimum value (' + auctionerMinimumValue + '). No more auction for thrifty guests!';
				do start_conversation (to: interestedGuests, protocol: 'fipa-propose', performative: 'cfp', contents: ['Stop']);
				interestedGuests <- [];
			}
		}

	}
	
	reflex sendAuctionInfo when: auctionRunning and time >= 50 and !empty(interestedGuests){
		if(auctionType = "Dutch")
		{
			write name + ' sends the offer of ' + auctionerDutchPrice +' krona to participants';
			do start_conversation (to: interestedGuests, protocol: 'fipa-propose', performative: 'propose', contents: ['Buy my merch', auctionType, auctionerDutchPrice]);
		}
//		
	}	
}// Auctioner

experiment main type: gui
{
	
	output
	{
		display map type: opengl
		{
			species Guest;
			species AuctionMaster;
			species Auctioner;
		}
	}
}
