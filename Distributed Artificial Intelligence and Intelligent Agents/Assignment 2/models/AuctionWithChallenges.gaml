/**
* Name: Auction
* Based on the internal empty template. 
* Author: rafat
* Tags: 
*/


model Auction

global {
	list<string> productList <- ["Laptops", "Mobile", "CD", "Cloths"];
	float distanceCoveredWithMemory;
	float distanceCoveredWithoutMemory;
	
	point pubLocation <- {15,12.5};
	point juiceParlorLocation <- {85,12.5};
	point restaurantLocaiton <- {15,87.5};
	
	point informationCenterLocation <- {50, 50};
	point auctionCenterLocation <- {45,20};
	
	Merchant0 M0;
	Merchant1 M1;
	Merchant2 M2;
	
	Consumer0 C0;
	Consumer1 C1;
	Consumer2 C2;
	Consumer3 C3;
	Consumer4 C4;
	bool endauction;
	
	
	init
	{
		create Entrance with: (location: {1,50});
		create Pub with: (location : pubLocation);
		create JuiceParlor with: (location: juiceParlorLocation);
		create Restaurant with: (location: restaurantLocaiton);
		//create Auctioneer with: (location: point(25, 75));
		//create Auctioneer with: (location: point(75, 75));
		
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
		
		create AuctionCentre number: 1
		{
			location <- {50,20};
		}
		
		create Merchant number: 3
		{
			reachedauctioncentre <- false;
			startofauction <- false;
			notsold <- false;
			endauction <- false;
			consumer <- 0;
			baseprice <- 500.0;
			startprice <- 999.0;
			location <- {50,50};
		}
		
		create Merchant0 number: 1 returns: m0
		{
			reachedauctioncentre <- false;
			startofauction <- false;
			notsold <- false;
			endauction <- false;
			consumer <- 0;
			baseprice <- 500.0;
			startprice <- 999.0;
			location <- {50,50};
		}
		create Merchant1 number: 1 returns: m1
		{
			reachedauctioncentre <- false;
			startofauction <- false;
			notsold <- false;
			endauction <- false;
			consumer <- 0;
			baseprice <- 500.0;
			startprice <- 999.0;
			location <- {0,50};
		}
		create Merchant2 number: 1 returns: m2
		{
			reachedauctioncentre <- false;
			startofauction <- false;
			notsold <- false;
			endauction <- false;
			consumer <- 0;
			baseprice <- 500.0;
			startprice <- 999.0;
			location <- {70,50};
		}
		create Consumer0 number: 1 returns: c0
		{
			updatepoint <- false;
			mad <- 0;
			wonauction <- false;
			reachedauctioncentre <- false;
			endauction <- false;
			offeredprice <- '';
			startofauction <- false;
			begin <- false;
			buyprice <- rnd(399.0, 799.0, 100.0);
			dist <- 1;
			targetpoint <- {rnd(100),rnd(100)};
			auctionpoint <- {rnd(48,54),rnd(16,24)};
			location <- {rnd(100),rnd(100)};
		}
		create Consumer1 number: 1 returns: c1
		{
			updatepoint <- false;
			mad <- 0;
			wonauction <- false;
			reachedauctioncentre <- false;
			endauction <- false;
			offeredprice <- '';
			startofauction <- false;
			begin <- false;
			buyprice <- rnd(399.0, 799.0, 100.0);
			dist <- 1;
			targetpoint <- {rnd(100),rnd(100)};
			auctionpoint <- {rnd(48,54),rnd(16,24)};
			location <- {rnd(100),rnd(100)};
		}
		create Consumer2 number: 1 returns: c2
		{
			updatepoint <- false;
			mad <- 1;
			wonauction <- false;
			reachedauctioncentre <- false;
			endauction <- false;
			offeredprice <- '';
			startofauction <- false;
			begin <- false;
			buyprice <- rnd(399.0, 799.0, 100.0);
			dist <- 1;
			targetpoint <- {rnd(100),rnd(100)};
			auctionpoint <- {rnd(48,54),rnd(16,24)};
			location <- {rnd(100),rnd(100)};
		}
		create Consumer3 number: 1 returns: c3
		{
			updatepoint <- false;
			mad <- 0;
			wonauction <- false;
			reachedauctioncentre <- false;
			endauction <- false;
			offeredprice <- '';
			startofauction <- false;
			begin <- false;
			buyprice <- rnd(399.0, 799.0, 100.0);
			dist <- 1;
			targetpoint <- {rnd(100),rnd(100)};
			auctionpoint <- {rnd(48,54),rnd(16,24)};
			location <- {rnd(100),rnd(100)};
		}
		create Consumer4 number: 1 returns: c4
		{
			updatepoint <- false;
			mad <- 1;
			wonauction <- false;
			reachedauctioncentre <- false;
			endauction <- false;
			offeredprice <- '';
			startofauction <- false;
			begin <- false;
			buyprice <- rnd(399.0, 799.0, 100.0);
			dist <- 1;
			targetpoint <- {rnd(100),rnd(100)};
			auctionpoint <- {rnd(48,54),rnd(16,24)};
			location <- {rnd(100),rnd(100)};
		}
		
		M0 <- m0 at 0;
		M1 <- m1 at 0;
		M2 <- m2 at 0;
		
		C0 <- c0 at 0;
		C1 <- c1 at 0;
		C2 <- c2 at 0;
		C3 <- c3 at 0;
		C4 <- c4 at 0;
		
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
species Merchant skills: [fipa, moving]{
	// Assuming each auctioneer sells everything 
	list<int> sells <- range(0, length(productList)); //need to implement
	float baseprice;
	float startprice;
	int consumer;
	bool notsold;
	bool startofauction;
	bool reachedauctioncentre;
	reflex go_for_auction when: reachedauctioncentre = false {
		do goto target:auctionCenterLocation speed: 5.0;
		if(location = auctionCenterLocation)
		{
			write self.name + ': Went to auction center';
			reachedauctioncentre <- true;
			startofauction <- true;
			notsold <- true;
		}
	}
	reflex start_of_auction when: startofauction = true {
		write self.name + ': INFORM';
		do start_conversation with: [ to :: [C0], protocol :: 'fipa-contract-net', performative :: 'inform', contents :: [self.name + ' is at the auction center. Auction is starting now.'] ];
		do start_conversation with: [ to :: [C1], protocol :: 'fipa-contract-net', performative :: 'inform', contents :: [self.name + ' is at the auction center. Auction is starting now.'] ];
		do start_conversation with: [ to :: [C2], protocol :: 'fipa-contract-net', performative :: 'inform', contents :: [self.name + ' is at the auction center. Auction is starting now.'] ];
		do start_conversation with: [ to :: [C3], protocol :: 'fipa-contract-net', performative :: 'inform', contents :: [self.name + ' is at the auction center. Auction is starting now.'] ];
		do start_conversation with: [ to :: [C4], protocol :: 'fipa-contract-net', performative :: 'inform', contents :: [self.name + ' is at the auction center. Auction is starting now.'] ];
		startofauction <- false;
	}
	reflex call_for_proposals when: notsold = true {
		write self.name + ': CALL FOR PROPOSAL';
		write self.name + ': Selling for the price: '+ startprice + ' Kr.';
		if consumer = 0{
			do start_conversation with: [ to :: [C0], protocol :: 'fipa-contract-net', performative :: 'cfp', contents :: ['Selling for the price: '+ startprice + ' Kr.'] ];
			notsold <- false;
		}
		else if consumer = 1{
			do start_conversation with: [ to :: [C1], protocol :: 'fipa-contract-net', performative :: 'cfp', contents :: ['Selling for the price: '+ startprice + ' Kr.'] ];
			notsold <- false;
		}
		else if consumer = 2{
			do start_conversation with: [ to :: [C2], protocol :: 'fipa-contract-net', performative :: 'cfp', contents :: ['Selling for the price: '+ startprice + ' Kr.'] ];
			notsold <- false;
		}
		else if consumer = 3{
			do start_conversation with: [ to :: [C3], protocol :: 'fipa-contract-net', performative :: 'cfp', contents :: ['Selling for the price: '+ startprice + ' Kr.'] ];
			notsold <- false;
		}
		else{
			do start_conversation with: [ to :: [C4], protocol :: 'fipa-contract-net', performative :: 'cfp', contents :: ['Selling for the price: '+ startprice + ' Kr.'] ];
			notsold <- false;
		}
	}
	reflex read_proposal when: !(empty(proposes)) and endauction = false {
		loop p over: proposes {
			write self.name + ': Proposal recieved: ' + string(p.contents);
			if string(p.contents) = '[\'' + string(startprice) + '\']'{
				write self.name + ': Its a deal';
				do accept_proposal with: [message :: p, contents :: ['Its a deal.']];
			}
			else{
				write self.name + ': No deal';
				do reject_proposal with: [message :: p, contents :: ['No deal.']];
				if consumer < 4{
					consumer <- consumer + 1;
				}
				else{
					if startprice >= baseprice{
						startprice <- startprice - 100.0;
						consumer <- 0;
						notsold <- true;
					}
					else{
						do end_conversation with: [message :: cfps at 0, contents :: ['Auction cancelled.']];
						endauction <- true;
					}
				}
				notsold <- true;
			}
		}
	}
	reflex auction_ended when: endauction = true {
		do goto target:{0,50} speed: 5.0;
		if(location distance_to({0,50}) < 1)
		{
			do die;
		}
	}
}

species Merchant0 parent: Merchant
{
	
	aspect default{ 
    	draw cone3D(1.5,2.5) at: location color: #green ;
    	draw sphere(1) at: location + {0, 0, 2} color: #orange ;
    }
}

species Merchant1 parent: Merchant
{
	
	aspect default{ 
    	draw cone3D(1.5,2.5) at: location color: #green ;
    	draw sphere(1) at: location + {0, 0, 2} color: #orange ;
    }
}

species Merchant2 parent: Merchant
{
	
	aspect default{ 
    	draw cone3D(1.5,2.5) at: location color: #green ;
    	draw sphere(1) at: location + {0, 0, 2} color: #orange ;
    }
}

species Consumer skills: [fipa, moving]{
	int mad;
	int dist;
	float buyprice;
	string offeredprice;
	bool begin;
	bool startofauction;
	bool reachedauctioncentre;
	point targetpoint;
	point auctionpoint;
	point securitypoint;
	bool wonauction;
	bool updatepoint;
	
	reflex roamaround when: (empty(informs)) and (reachedauctioncentre = false or (endauction = true and wonauction = false))
	{
		
		do wander speed: 5.0;
		if endauction = true and wonauction = false {
			updatepoint <- true;
		}
	}
	
	reflex go_for_auction when: reachedauctioncentre = false and !(empty(informs)) {
		do goto target:auctionpoint speed: 5.0;
		if(location = auctionpoint)
		{
			reachedauctioncentre <- true;
			startofauction <- true;
		}
	}
	reflex read_inform_message when: !(empty(informs)) and startofauction = true {
		loop i over: informs {
			write self.name + ': Information recieved: ' + string(i.contents);
		}
		startofauction <- false;
	}
	reflex read_cfp_message when: !(empty(cfps)) and reachedauctioncentre = true {
		loop c over: cfps {
			if offeredprice != string(c.contents){
				write self.name + ': Cfp recieved: ' + string(c.contents);
				write self.name + ': I am willing to buy for ' + buyprice + ' Kr.';
				do propose with: [message :: c, contents :: [string(buyprice)]];
				offeredprice <- string(c.contents);
			}
		}
	}
	reflex proposal_accepted when: !(empty(accept_proposals)) {
		loop ap over: accept_proposals {
			write self.name + ': Acceptance recieved: ' + string(ap.contents);
			write self.name + ': OK.';
			do end_conversation with: [message :: ap, contents :: ['OK.']];
			endauction <- true;
			wonauction <- true;
		}
	}
	reflex proposal_rejected when: !(empty(reject_proposals)) {
		loop rp over: reject_proposals {
			write self.name + ': Rejection recieved: ' + string(rp.contents);
			write self.name + ': OK.';
			do end_conversation with: [message :: rp, contents :: ['OK.']];
		}
	}
	reflex auction_ended when: endauction = true and wonauction = true {
		write self.name + ': I have won.';
		do goto target:{0,50} speed: 5.0;
		if(location distance_to({0,50}) < 1)
		{
			do die;
		}
		endauction <- true;
		wonauction <- false;
		
	}
	aspect default{ 
    	draw cone3D(1.5,2.5) at: location color: #white ;
    	draw sphere(1) at: location + {0, 0, 2} color: #black ;
    }
}
species Consumer0 parent: Consumer
{
	
	aspect default{ 
    	draw cone3D(1.5,2.5) at: location color: #white ;
    	draw sphere(1) at: location + {0, 0, 2} color: #black ;
    }
}
species Consumer1 parent: Consumer
{
		aspect default{ 
    	draw cone3D(1.5,2.5) at: location color: #white ;
    	draw sphere(1) at: location + {0, 0, 2} color: #black ;
    }
}
species Consumer2 parent: Consumer
{
		aspect default{ 
    	draw cone3D(1.5,2.5) at: location color: #white ;
    	draw sphere(1) at: location + {0, 0, 2} color: #black ;
    }
}
species Consumer3 parent: Consumer
{
	aspect default{ 
    	draw cone3D(1.5,2.5) at: location color: #white ;
    	draw sphere(1) at: location + {0, 0, 2} color: #black ;
    }
}
species Consumer4 parent: Consumer
{
		aspect default{ 
    	draw cone3D(1.5,2.5) at: location color: #white ;
    	draw sphere(1) at: location + {0, 0, 2} color: #black ;
    }
}

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
			species GuestWithNoMemory;
			species GuestWithMemory;
			species SecurityGuard;
			species Merchant;
			species AuctionCentre;
			species Consumer0;
			species Consumer1;
			species Consumer2;
			species Consumer3;
			species Consumer4;
		}
		display chart
		{
			chart "Agent displacements"
			{
				data "Agents with memory" value: distanceCoveredWithMemory color: #green;
				data "Agents without memory" value: distanceCoveredWithoutMemory color: #red;
			}
		}
	}
}

