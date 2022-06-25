/**
* Name: Auction
* Based on the internal empty template. 
* Author: rafat
* Tags: 
*/


model Auctions

/* Model definition */
global {
	
	list<string> merch <- ["T-shirt", "CD", "Pillow", "iPhone cover", "Bracelet"];
	int minPrice <- 100;
	int maxPrice <- 1000;

	init {
		create Guest number: 10 
		{
		informed <- false;
		WCPoint <- {0,0};
		wc <- rnd(200)+100;
		wait <- 20;
		}

		create Auctioneer with: (location: point(50, 25));
		create WC number:1 
		{
		location <- {25,25};
		}
		create InformationCenter number: 1
		{
			
			location <- {10,50};
		}
		
		
		create Auctioneer with: (location: point(25, 75));
		create Auctioneer with: (location: point(75, 75));
		
	}
	
}
species InformationCenter{
	
	aspect default{
    	draw cylinder(8,8) at: location color: #red lighted: bool(1) ;
    }
}
species WC{
	aspect default{
    	draw box(10,5,2) at: location color: #blue lighted: bool(1) ;
    }
}

species Guest skills:[moving, fipa] {
	
	point targetPoint;
	point WCPoint;
	rgb myColor <- #blue;
	int wc;
	int wait;
	bool informed;
	
	int interest <- rnd(length(merch));
	int acceptablePrice <- rnd(minPrice, maxPrice / 2);
	
	aspect default{
		draw sphere(1.5) at: location color: myColor;
	}
	reflex gotoInfo when: (wc <= 0 and informed = false)
	{


		if wc <= 0
		{
			
			
				targetPoint <- {10,50}; // 


		
		}

		else
		{
			targetPoint <- (location + {rnd(20),rnd(20)});
		}
		//do goto target:{50,80} speed: 3.0;
		if(location = {10,50})
		{
			if wait <= 0
			{
				write name + "says: I need to go bathroom.";
				write name + "says: Thanks, I got it!";
				informed <- true;
				wait <- 10;
			}
			wait <- wait - 1;
		}
	}
	reflex fromInformationCentre when: informed = true
	{

		if wc <= 0
		{
			if informed = true
			{
				// Guided from information centre.
				self.WCPoint <- {25,25}; // Memorize
				targetPoint <- self.WCPoint;
			}
			else
			{
				write name + "says: OMG.. I've to pee/poo.";
				targetPoint <- self.WCPoint;
			}
			do goto target:targetPoint;

			if(location = targetPoint)
			{
				if wait <= 0
				{
					write self.name + "says:I am relieved!";
					self.wc <- rnd(200)+100;
					self.informed <- false;
					self.wait <- 20;
					targetPoint <- {rnd(100),rnd(100)};
					//targetPoint <- nil;
					
				}
				wait <- wait - 1;
			}
		}
		else
		{
			do goto target:{rnd(100),rnd(100)};
			
		}
	}
	reflex fromWC when: (targetPoint != nil and informed=false and wc != 0)
	{
		if (location distance_to(self.targetPoint) < 3)
		{
		myColor <- #blue;
		targetPoint <- nil;
		do wander;
		wc <- wc-1;
		
		if informed =true
		{
			self.WCPoint <- {25,25};
			if ((location distance_to(self.WCPoint) < 3) )
			{
					self.WCPoint <- {0,0};
					informed <- false;
			}	
		}
		
		}	
	}
	reflex beIdle when: (targetPoint = nil and wc != 0) 
	{
		myColor <- #blue;
		do wander;
		wc <- wc-1;
		if informed =true
		{
			self.WCPoint <- {25,25};
			if ((location distance_to(self.WCPoint) < 3) )
			{
					self.WCPoint <- {0,0};
					informed <- false;
			}	
		}
	}
	
	
	reflex moveToTarget when: targetPoint != nil {
		myColor <- #purple;
		do goto target:targetPoint;
	}
	
	reflex receive_inform_messages when: !empty(informs) and wc != 0 {
		message m <- informs[0];
		if rnd(0, 10) = 2 {
			// Not understood
			write '\t' + name + ' does not understand.';
			do refuse with: [ message :: m, contents :: ['not-understood'] ];
			return;
		}
		if (self.interest != m.contents) {
			// Not interested
			write '\t' + name + ' is not interested.';
			do refuse with: [ message :: m, contents :: ['not-interested'] ];
		} else if (self.interest = m.contents) {
			// Interested
//			write '\t' + name + ' is interested.';
			do agree with: [ message :: m, contents :: ['interested'] ];
			// Propose
			int productID <- self.interest;
			acceptablePrice <- acceptablePrice + (acceptablePrice/100 * 10);
			
			do propose with: [ message :: m, contents :: [productID, acceptablePrice] ];
		}
	}
	
}

species Auctioneer skills:[fipa] {
	
	int auctionRange <- 15;
	
	// Assuming each auctioneer sells everything 
	list<int> sells <- range(0, length(merch));
	
	int participants <- 0;

	bool auctioning;
	
	int productID;
	int lowestPrice;	// lowest price that the auctioneer is willing to accept 
	int price;
	
	aspect default {
		draw sphere(2) at: location color: #brown;
		draw circle(auctionRange) at: location color: #orange;
	}
	
	// Start auction
	reflex start_auction when: (time mod 60 = 0) and !auctioning {
		// Every minute
		auctioning <- true;
		list<Guest> possibleParticipants <- Guest at_distance(auctionRange);
		
		if (length(possibleParticipants) = 0) {
			auctioning <- false;
			return;	// Do not start if no one in range
		}
		
		productID <- rnd(length(merch) - 1 );
		price <- rnd(minPrice, maxPrice); 
		lowestPrice <- (price / 2);
		//acceptablePrice <- (price / 3 * 2);
		
		write '(Time ' + time + '): ' + name + ' initiates an auction for ' + merch[productID] + '.';
		write '\tThe initial price is ' + price + ' euros.';
		
		participants <- length(possibleParticipants);
		
		
		// Stop agents
		loop p over: possibleParticipants {
			p.targetPoint <- p.location;
		}
		// inform-start-of-auction 
		do start_conversation with: [to :: possibleParticipants, protocol :: 'fipa-contract-net', performative :: 'inform', contents ::  [productID, price] ];
	}
	
	// Receive refute message
	reflex receive_refuse_messages when: !empty(refuses) {		
		loop m over: refuses {
			write '\t' + m.sender + ' left the auction';
				do end_conversation with: [ message :: m, contents :: [] ];
				participants <- participants - 1;
				Guest(m.sender).targetPoint <- nil;	// Frees agent;
		}
	}
	
	reflex receive_propose_messages when: !empty(proposes) {
    int prices <- 0;
    int n <- 0;
    bool sold <- false;
    list<Guest> interestedPeople <- [];
    loop m over: proposes {
      list<unknown> c <- m.contents;
      int x <- int(c[1]);
      int p <- x;
      write("\t" + Guest(m.sender).name + " proposed " + p + " euros.");
      if (p >= lowestPrice) {
        // SOLD
        int y <- int(c[0]);        
        write("\t" + Guest(m.sender).name + " bought " + merch[y] + " for " + p + " euros.");
        sold <- true;
        do accept_proposal with: [ message :: m, contents :: [] ];
        interestedPeople <- interestedPeople union [Guest(m.sender)];
      } else {
        do reject_proposal with: [ message :: m, contents :: [] ];
        interestedPeople <- interestedPeople union [Guest(m.sender)]; 
      }
      do end_conversation with: [ message :: m, contents :: [] ];
      prices <- prices + int(x);
      n <- n + 1;
    }
    // Compute new price
    int avgPrice <- prices / n;
    int newPrice <- rnd(avgPrice, price);
    price <- newPrice; 
    // New offer
    if (not sold) {
      do start_conversation with: [to :: interestedPeople, protocol :: 'fipa-contract-net', performative :: 'inform', contents ::  [productID, price] ];
    } else if (sold or price < lowestPrice) {
      // SOLD
       loop p over: interestedPeople {
         p.targetPoint <- nil;
       }
       participants <- 0;
       write("\tThe auction by " + name + " is over.");
    }
  }
	reflex end_auction when: auctioning and participants = 0 {
		auctioning <- false;
	}	
}

experiment main type: gui {
	output {
		display map type: opengl {
			species Guest;
			species InformationCenter;
			species WC;
			species Auctioneer;
		}
	}
}

