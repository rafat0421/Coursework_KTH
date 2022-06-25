/**
* Name: FestivalWithAuction
* Based on the internal empty template. 
* Author: rafat
* Tags: 
*/


model FestivalWithAuction

global {
	
	float distanceCoveredWithMemory;
	float distanceCoveredWithoutMemory;
	
	point pubLocation <- {15,12.5};
	point juiceParlorLocation <- {85,12.5};
	point restaurantLocaiton <- {15,87.5};
	
	point informationCenterLocation <- {50, 50};
	
	//Rafat
	int guestNumber <- rnd(20)+20;
	
	// the rate at which guests grow hungry / thirsty
	// every reflex we reduce hunger / thirst by rnd(0,rate) * 0.1
	
	 /* Auction configs
	 */
	point auctionerMasterLocation <- {-10,50};
	list<string> items <- ["Cloths","Mobile","Laptop", "Gedgets"];
	
	// Time when auctioneers are created
	int auctionCreationMin <- 0;
	int auctionCreationMax <- 50;
	
	// Guest accepted price range min and max
	int guestAcceptedPriceMin <- 100;
	int guestAcceptedPriceMax <- 1500;
	
	// English auction configs
	// bid raise min and max
	int engAuctionRaiseMin <- 30;
	int engAuctionRaiseMax <- 60;
	// The initial price of the item to sell
	int auctionerEngPriceMin <- 0;
	int auctionerEngPriceMax <-1500;
	
	// Dutch auction configs
	// bid decrease min and max 
	int dutchAuctionDecreaseMin <- 5;
	int dutchAuctionDecreaseMax <- 15;
	// The initial price of the item to sell, set above the max price so that no guest immediately wins
	int auctionerDutchPriceMin <- 1504;
	int auctionerDutchPriceMax <-1600;
	// Minimum price of the item, if the bids go below this the auction fails
	int auctionerMinimumValueMin <- 90;
	int auctionerMinimumValueMax <- 300;
	
	list<string> auctionTypes <- ["Dutch", "English", "Sealed"];
	
	
	
	init
	{
		create Entrance with: (location: {1,50});
		create Pub with: (location : pubLocation);
		create JuiceParlor with: (location: juiceParlorLocation);
		create Restaurant with: (location: restaurantLocaiton);
		
		create InformationCenter number: 1
		{
			location <- informationCenterLocation;
			pubAddress <- pubLocation;
			juiceParlorAddress <- juiceParlorLocation;
			restaurantAddress <- restaurantLocaiton;
		}
		
		create BadGuest number: 0
		{
			location <- {rnd(100),rnd(100)};
			informed <- false;
			thirst <- rnd(50);
			hunger <- rnd(50);
			targetPoint <- nil;
		}
		
		create GuestWithNoMemory number: 0
		{
			location <- {rnd(100),rnd(100)};
			informed <- false;
			thirst <- rnd(50);
			hunger <- rnd(50);
			targetPoint <- nil;
			distanceCoveredWithoutMemory <- 0.0;
		}
		
		create GuestWithMemory number: 0
		{
			location <- {rnd(100),rnd(100)};                                                                                                                           
			thirst <- rnd(50);
			hunger <-rnd(50);
			targetPoint <- nil;
			pubAddress <- nil;
			juiceParlorAddress <- nil;
			restaurantAddress <- nil;
			distanceCoveredWithMemory <- 0.0;
		}
		
		create SecurityGuard number: 1
		{
			location <- {4,55};
		}
		
		create AuctionGuest number: guestNumber
		{
			// Each guest prefers a random item
			preferredItem <- items[rnd(length(items) - 1)];
		}
		
	}
}

species Entrance{
	aspect default{
    	draw box(2,15,2) at: location color: #violet lighted: bool(1) ;
    }
}

species Pub{
	aspect default{
    	draw box(10,5,2) at: location color: #blue lighted: bool(1) ;
    }
}

species JuiceParlor{
	aspect default{
    	draw box(10,5,2) at: location color: #yellow lighted: bool(1) ;
    }
}

species Restaurant{
	aspect default{
    	draw box(10,5,2) at: location color: #cyan lighted: bool(1) ;
    }
}

species SecurityGuard skills: [moving]{
	
	action killBadGuest(BadGuest badGuest) {
		//write self.name + " says: Bad guest reported, killing him!";
		do goto target: badGuest.location speed: 30.0;
		ask badGuest {
			do die;
		}
	}

	aspect default{
    	draw cylinder(2,2) at: location color: #black lighted: bool(1) ;
    }
}

species InformationCenter{
	point pubAddress;
	point restaurantAddress;
	point juiceParlorAddress;
	int distanceThreshold <- 5;
	
	aspect default{
    	draw pyramid(10) at: location color: #orange lighted: bool(1) ;
    }
  
    point getRandomDrinkingPlace {
    	if (rnd(0,1) = 0) {
    		return pubAddress;
    	}
    	return juiceParlorAddress;
    }
    
    reflex serviceGuestsWithMemory when: !empty(GuestWithMemory at_distance distanceThreshold) {
    	ask GuestWithMemory at_distance distanceThreshold {
    		if (self.thirsty())
    		{
				if (self.newGuest() or self.lastDrinkingPlace = nil) 
				{
					//If it is a new guest it is just given the address of the Juice Parlor
					//write self.name + " says: I just arrived, where can I go to get something to drink?";
					//write myself.name + " says: You can try the Juice Parlor." ;
					self.juiceParlorAddress <- myself.juiceParlorAddress;
				}
				else if (self.tryNewDrinkingPlace())
    			{
    				//write self.name + " says: I want to try another drinking place?";
    				//write myself.name + " says: You can try the Pub.";
					self.pubAddress <- myself.pubAddress;
    			}
    		}
    		else if (self.hungry()) 
    		{
    			//write self.name + " says: I am hungry, where can I go?";
    			//write myself.name + " says: You can try the Restaurant.";
    			self.restaurantAddress <- myself.restaurantAddress;
    		}
    	}
    }
    
    reflex serviceGuestsWithoutMemory when: !empty(GuestWithNoMemory at_distance distanceThreshold) {
    	ask GuestWithNoMemory at_distance distanceThreshold {
    		if (self.thirsty()) 
    		{
    			//write self.name + " says: I am thirsty, where can I go?";
    			//write myself.name + " says: You can try this place";
    			self.targetPoint <- myself.getRandomDrinkingPlace();
    			informed <- true;
    		}
    		else if (self.hungry()) 
    		{
    			//write self.name + " says: I am hungry, where can I go?";
    			//write myself.name + " says: You can try the Restaurant.";
    			self.targetPoint <- myself.restaurantAddress;
    			informed <- true;
    		}
    	}
    }
    
    reflex reportBadGuests when: !empty(BadGuest at_distance distanceThreshold) {
    	//write self.name + " says: Found a bad guest, reporting to security.";
    	ask BadGuest at_distance distanceThreshold {
    		ask SecurityGuard {
    			do killBadGuest(myself);
    		}
    	}
    }
}

species Guest skills: [moving] {
	int thirst;
	int hunger;
	bool informed;
	point targetPoint;
	
	rgb color <- #red;
	
	bool thirsty {
		return thirst <= 0;
	} 
	
    bool hungry {
    	return hunger <= 0;
    }
    
    bool needNutrition {
    	return thirsty() or hungry();
	}
	
	reflex movearound when: thirst > 0 and hunger > 0 and targetPoint = nil
	{
		//write self.name + " says: Wandering around";
		thirst <- thirst - 1;
		hunger <- hunger - 1;
		do wander speed: 3.0;
	}
	
	reflex goToInformationCenter when: needNutrition() and !informed
	{
		//write self.name + " says: Going to the information center.";
		do goto target:informationCenterLocation speed: 3.0;
	}
	
	reflex moveToTarget when: targetPoint != nil
	{
		//write self.name + " says: Moving to " + targetPoint;
		do goto target:targetPoint speed: 3.0;
	}
	
	reflex enterStore when: targetPoint != nil and location distance_to(targetPoint) < 2
	{
		//write self.name + " says: Just entered the store.";
		if (thirsty() and hungry()) {
			thirst <- rnd(100);
			hunger <- rnd(100);
		}
		else if (thirsty()) 
		{
			//write self.name + " says: Just drank :).";
			thirst <- rnd(100);
		}
		else 
		{
			//write self.name + " says: Just ate :).";
			hunger <- rnd(100);
		}
		informed <- false;
		targetPoint <- nil;
	}
}

species AuctionGuest skills: [moving, fipa]
{
	//Rafat: This is the price at which the guest will buy merch, set in the configs above
	bool wonAuction <- false;
	
	// Default color of guests
	rgb color <- #red;
	
	// This is the price at which the guest will buy merch, set in the configs above
	int guestMaxAcceptedPrice <- rnd(guestAcceptedPriceMin,guestAcceptedPriceMax);
	
	point target;
	// Which auction is guest participating in
	Auctioner targetAuction;
	
	// each guest prefers a single piece of merchandice
	string preferredItem;
	
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
	
	reflex listen_messages when: (!empty(cfps))
	{
		message requestFromInitiator <- (cfps at 0);
		list<string> tlist <- requestFromInitiator.contents;
		// the request's format is as follows: [String, auctionType, soldItem, ...]
		if(tlist[0] = 'Start' and tlist[1] = preferredItem)
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
				int newBid <- currentBid + rnd(engAuctionRaiseMin, engAuctionRaiseMax);
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
				write name + ": Too much for me, I'm out guyzz";
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
				do reject_proposal (message: requestFromInitiator, contents: ["I, " + name + ", already have a house full of crap, you scoundrel!"]);	
				targetAuction <- nil;
				target <- nil;
			}
		}
	}
	
	
	aspect default{
    	draw cylinder(1,1) at: location color: #red lighted: bool(1) ;
    	
    	if (wonAuction = true)
		{
			if(preferredItem = "branded backpacks")
			{
				//point backPackLocation <- location + point([2.1, 0.0, 2.0]);
				//backPackLocation <- backPackLocation.x + 1; 
				draw cube(1.2) at: location + point([2.1, 0.0, 2.0]) color: #purple;
			}
			else if(preferredItem = "heavenly hats")
			{
				//point hatLocation <- location + point([0.0, 0.0, 3.5]);
				draw pyramid(1.2) at: location + point([0.0, 0.0, 3.5]) color: #orange;
			}
			else if(preferredItem = "signed shirts")
			{
				//point shirtLocation <- location + point([0.0, 0.0, 1.0]);
				draw cylinder(2.01, 1.5) at: location + point([0.0, 0.0, 1.0]) color: #lime;
			}
			else if(preferredItem = "posh pants")
			{
				//point shirtLocation <- location + point([0.0, 0.0, 0.0]);
				draw cylinder(2.01, 1.5) at: location color: #pink;
			}
		}
    }
    
    
}

species BadGuest parent: Guest 
{
	aspect default{
    	draw cylinder(1,1) at: location color: #red lighted: bool(1) ;
    }
}

species GuestWithNoMemory parent: Guest 
{
	aspect default {
    	draw cylinder(1,1) at: location color: #blue lighted: bool(1) ;
    }
    
    
    reflex movearound when: thirst > 0 and hunger > 0 and targetPoint = nil
	{
		distanceCoveredWithoutMemory <- distanceCoveredWithoutMemory + 1;
		thirst <- thirst - 1;
		hunger <- hunger - 1;
		do wander speed: 3.0;
	}
	
    reflex goToInformationCenter when: needNutrition() and !informed
	{
		distanceCoveredWithoutMemory <- distanceCoveredWithoutMemory + 1;
		do goto target:informationCenterLocation speed: 3.0;
	}
	
	reflex moveToTarget when: targetPoint != nil
	{
		distanceCoveredWithoutMemory <- distanceCoveredWithoutMemory + 1;
		do goto target:targetPoint speed: 3.0;
	}
}

species GuestWithMemory parent: Guest {
	point juiceParlorAddress;
	point pubAddress;
	point restaurantAddress;
	point lastDrinkingPlace;
	int drinkingPlaceVisited <- 1;
 
	aspect default{ 
    	draw cylinder(1,1) at: location color: #yellow lighted: bool(1) ;
    }
    
    //The guest is new
    bool newGuest {
    	return (juiceParlorAddress = nil) and (pubAddress = nil) and (restaurantAddress = nil);
    }
    
    //The guest is thirsty but only know the location of the restaurant
    bool requestDrinkingPlace {
    	return thirsty() and (juiceParlorAddress = nil) and (pubAddress = nil) and (restaurantAddress != nil);
    }
    
    point getKnownDrinkingPlace {
    	if (juiceParlorAddress != nil) 
    	{
    		return juiceParlorAddress;
    	}
    	else if (pubAddress != nil)
    	{
    		return pubAddress;
    	}
    	return nil;
    }
    
    //The guest has been to the same drinking place six times and wants to try another place
    bool tryNewDrinkingPlace {
    	if (lastDrinkingPlace != nil and drinkingPlaceVisited >= 6) 
    	{
    		drinkingPlaceVisited <- 1;
    		return true;
    	}
   		else if (lastDrinkingPlace != nil) 
   		{
   			drinkingPlaceVisited <- drinkingPlaceVisited + 1;
   		}
   		return false;
	}
	
	//Need to go to information center for restaurant location
	bool findRestaurantLocation {
		return hungry() and restaurantAddress = nil;
	}
	
	//Get the not lastDrinkingPlace 
	point getNotLastDrinkingPlace {
		if (lastDrinkingPlace = pubAddress) 
		{
			return juiceParlorAddress;
		}
		else if (lastDrinkingPlace = juiceParlorAddress) 
		{
			return pubAddress;
		}
		return nil;
	}
 
    reflex goToInformationCenter when: (thirsty() and (tryNewDrinkingPlace() or requestDrinkingPlace() or newGuest()) or findRestaurantLocation())
    {
    	//write self.name + " says: Going to information center";
    	distanceCoveredWithMemory <- distanceCoveredWithMemory + 1;
    	do goto target:informationCenterLocation speed: 3.0;
    }
    
    reflex moveToTarget when: needNutrition()
    {
    	distanceCoveredWithMemory <- distanceCoveredWithMemory + 1;
    	if (hungry()) {
    		//The guest already knows the address of the restaurant
    		if (restaurantAddress != nil) {
    			targetPoint <- restaurantAddress;
    			//write self.name + " says: I am hungry, moving to " + targetPoint;
    			do goto target:targetPoint speed: 3.0;
    		}
    	}
    	else if (thirsty()) 
    	{
    		//The guest knows the different drinking places and wants to try a different one from the last
    		if (tryNewDrinkingPlace() and (getNotLastDrinkingPlace() != nil)) {
    			point newDrinkingPlace <- getNotLastDrinkingPlace();
    			targetPoint <- newDrinkingPlace;
				lastDrinkingPlace <- newDrinkingPlace;
    			do goto target:targetPoint speed: 3.0;
    		}
    		//The guest doesnt want to try another place 
    		else if(!tryNewDrinkingPlace() and lastDrinkingPlace != nil) {
    			targetPoint <- lastDrinkingPlace;
    			do goto target:targetPoint speed: 3.0;
    		}
    		//The guest hasnt been to a drinking place 
    		else if(getKnownDrinkingPlace() != nil) {
    			targetPoint <- getKnownDrinkingPlace();
    			lastDrinkingPlace <- targetPoint;
    			do goto target:targetPoint speed: 3.0;
    		}
    	}
    }
}

species AuctionCentre{
	aspect default{
    	draw square(10) at: location color: #violet lighted: bool(1) ;
    }
}

//start Rafat
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
	
	/*
	 * For flashing and changing color
	 */
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
	
	/*
	 * This creates the auctioners within the set time limits from the beginning.
	 * auctionCreationMin and auctionCreationMax set at the top
	 */
	reflex createAuctioners when: !auctionersCreated and time rnd(0, 100)
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
}

/*
 * TODO: document
 * TODO: maybe auctioners buy their own wares from a central storage - use the other auctions for this?
 */
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
		draw pyramid(mySize) color: myColor;
	}
	
	/*
	 * For flashing and changing size
	 */
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
	
	/*
	 * For rushing to the field
	 */
	 reflex goToLocation when: targetLocation != nil
	 {
	 	if(location distance_to targetLocation <= 0.1)
	 	{
	 		targetLocation <- nil;
	 		write name + " has reached targetLocation";
	 	}
	 	else
	 	{
	 		do goto target: targetLocation speed: 2;	
	 		myColor <- #gray;
	 		mySize <- 5;
	 	}
	 }
	
	/*
	 * Send out the first auction message to all guest after a random amount of time
	 * Interested guests will answer and be added to interestedGuests
	 * The auction will start once the guests have gathered
	 * 
	 * startAnnounced is here to ensure we don't spam the announcement message
	 * TODO: set starting time to something more interesting
	 */
	reflex sendStartAuction when: !auctionRunning and time >= 90 and targetLocation = nil and !startAnnounced
	{
		write name + " starting " + auctionType + " soon";
		do start_conversation (to: list(Guest), protocol: 'fipa-propose', performative: 'cfp', contents: ['Start', soldItem]);
		startAnnounced <- true;
	}
	
	/*
	 * sets auctionStarted to true when interestedGuests are within a distance of 13 to the auctioner.
	 */
	reflex guestsAreAround when: !auctionRunning and !empty(interestedGuests) and (interestedGuests max_of (location distance_to(each.location))) <= 13
	{
		write name + " guestsAreAround";
		auctionRunning <- true;
	}

	/*
	 * Dutch auction: auctioner sends a propose message and guests can reply with accept or reject messages. The auction ends with the first accept.
	 */
	reflex receiveAcceptMessages when: auctionRunning and !empty(accept_proposals)
	{
		if(auctionType = "Dutch")
		{
			write name + ' receives accept messages';
			
			loop a over: accept_proposals {
				write name + ' got accepted by ' + a.sender + ': ' + a.contents;
				do start_conversation (to: a.sender, protocol: 'fipa-propose', performative: 'cfp', contents: ['Winner']);
			}
			targetLocation <- auctionerMasterLocation;
			auctionRunning <- false;
			//end of auction
			do start_conversation (to: interestedGuests, protocol: 'fipa-propose', performative: 'cfp', contents: ['Stop']);
			interestedGuests <- [];
		}
	}

	reflex getProposes when: (!empty(proposes))
	{
		if(auctionType = "Sealed")
		{
			//In Sealed Auction, the participants send proposes to the auctioner. The highest bid wins right away.
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
		else if(auctionType = "English")
		{
			//In English Auction, the participants send proposes to the auctioner which sets the current highest bid and the auction goes on.
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
	/*
	 * Reject messages are used in Dutch and English auctions.
	 * Dutch: Starting from high bid and goes on as long as everybody rejects the proposal. Here, we decrese the price of the item.
	 * If the price goes below the minimum expected price, the auction ends.
	 * English: Reject messages mean that participants don't wish to bid more and are out of the auction.
	 * If everyone is out or just one person left, the auction ends.
	 */
	reflex receiveRejectMessages when: auctionRunning and !empty(reject_proposals)
	{
		if(auctionType = "Dutch")
		{
			write name + ' receives reject messages';
			
			auctionerDutchPrice <- auctionerDutchPrice - rnd(dutchAuctionDecreaseMin, dutchAuctionDecreaseMax);
			if(auctionerDutchPrice < auctionerMinimumValue)
			{
				targetLocation <- auctionerMasterLocation;
				auctionRunning <- false;

				write name + ' price went below minimum value (' + auctionerMinimumValue + '). No more auction for thrifty guests!';
				do start_conversation (to: interestedGuests, protocol: 'fipa-propose', performative: 'cfp', contents: ['Stop']);
				interestedGuests <- [];
			}
		}
		else if(auctionType = "English")
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
					write name + ' bid ended. No more auctions for poor people!';
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
	/*
	 * Dutch: every iteration, it sends the decreased price of the item to the participants which they can accept of reject
	 * English: every iteration, tells guests about the current highest bid that they need to outbid
	 * Sealed: Start of the auction which is only one iteration
	 */
	reflex sendAuctionInfo when: auctionRunning and time >= 50 and !empty(interestedGuests){
		if(auctionType = "Dutch")
		{
			write name + ' sends the offer of ' + auctionerDutchPrice +' kr to participants';
			do start_conversation (to: interestedGuests, protocol: 'fipa-propose', performative: 'propose', contents: ['Bid for Dutch', auctionType, auctionerDutchPrice]);
		}
		else if(auctionType = "English")
		{
			write 'Auctioner ' + name + '(English Auction): current bid is: ' + currentBid + '.';
			do start_conversation (to: interestedGuests, protocol: 'fipa-propose', performative: 'cfp', contents: ["Bid for English", currentBid]);
		}
		else if(auctionType = "Sealed")
		{
			write 'Auctioner ' + name + '(Sealed Auction): place your bid.';
			do start_conversation (to: interestedGuests, protocol: 'fipa-propose', performative: 'cfp', contents: ['Bid For Sealed']);
		}
	}	
}// Auctioner

experiment main type: gui {
	output {
		display map type: opengl
		{
			image '../icons/Background.jpg' ;
			species InformationCenter;
			species Entrance;
			species Pub;
			species JuiceParlor;
			species Restaurant;
			species BadGuest;
			species AuctionGuest;
			species GuestWithNoMemory;
			species GuestWithMemory;
			species SecurityGuard;
			species AuctionerMaster;
			species Auctioner;
		}
		//display chart
		//{
		//	chart "Agent displacements"
		//	{
		//		data "Agents with memory" value: distanceCoveredWithMemory color: #green;
		//		data "Agents without memory" value: distanceCoveredWithoutMemory color: #red;
		//	}
		//} 
	}
}

