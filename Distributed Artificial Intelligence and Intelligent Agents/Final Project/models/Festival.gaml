/**
* Name: Festival
* Based on the internal empty template. 
* Author: rafat
* Tags: 
*/


model Festival

/* Insert your model definition here */

global {
	list<string> guest_type <- ['party', 'chill', 'rock_fan', 'disco_dancer', 'pop_queen', 'meat_lover', 'vegan'];
	list<point> place_location <- [{10,10},{10,30},{10,60},{10,90},{30,10},{30,30},{30,60},{30,90},{60,10},{60,30},{60,60},{60,90},{90,10},{90,30},{90,60},{90,90}];
	list<string> place_type <- ['Dancefloor', 'Resturant', 'Pub', 'Rock_Concert', 'Pop_Concert'];
	int type_assigned <- 0;
	list<place> placeList <- [];
	list<place> placeListDancefloor <- [];
	list<place> placeListResturant <- [];
	list<place> placeListPub <- [];
	list<place> placeListRock <- [];
	list<place> placeListPop <- [];
	int accept_rate <- 0;
	int reject_rate <- 0;
	
	init {
		create guest number: 50;
		create place number: 16;
	}
}

species place {

	string type;
	rgb color;
	bool initialized <- false;
	
	aspect base {
		if !self.initialized{
			self.initialized <- true;
			add self to: placeList;
			//Assign type
			self.type <- place_type[rnd(0, length(place_type)-1)];
			self.location <- place_location[type_assigned];
			type_assigned <- type_assigned + 1;
			if self.type = 'Dancefloor'{
				add self to: placeListDancefloor;
				self.color <- #lawngreen;
			} else if self.type = 'Resturant'{
				add self to: placeListResturant;
				self.color <- #cyan;
			} else if self.type = 'Pub'{
				add self to: placeListPub;
				self.color <- #orange;
			} else if self.type = 'Rock_Concert'{
				add self to: placeListRock;
				self.color <- #green;
			} else if self.type = 'Pop_Concert'{
				add self to: placeListPop;
				self.color <- #red;
			}
		}
		draw pyramid(4) at: self.location color: #gray;
		draw string(self.type) at: self.location + {-6, 6} color: self.color font: font('Default', 12, #bold);
	}
}

species guest skills: [moving, fipa] {
	
	//Personal traits
	int kind <- rnd(0,10);
	int sympathetic <- rnd(0,10);
	int generous <- rnd(0,10);
	
	//Guest Types
	float party_people;
	float chill_people;
	float rock_fan_people;
	float disco_dancer_people;
	float pop_queen_people;
	float meat_lover_people;
	float vegan_people;
	
	int time <- 0 update: time + 1;
	int time_limit <- rnd(200,400);
	bool limit_interactions <- false;
	
	string type <- guest_type[rnd(0, length(guest_type)-1)];
	rgb color;
	point targetPoint;
	bool initialized <- false;
	bool decider;


	list<guest> guestList<- [];
	bool initiater <- false;
	bool participant <- false;
	bool dialoge <- false;
	guest participant_name <- nil;
	list<guest> initiater_name <- nil;
	
	
	aspect base {
		if self.initialized = false {
			if self.type = 'party' {
				self.color <- #lawngreen;
				self.party_people  <- 1.0;
				self.chill_people  <- 0.5;
				self.rock_fan_people  <- with_precision(rnd(0.0,1.0),1);
				self.disco_dancer_people  <- 0.2;
				self.pop_queen_people  <- with_precision(rnd(0.0,1.0),1);
				self.meat_lover_people <- 1.0;
				self.vegan_people <- 0.0;
			} else if self.type = 'chill' {
				self.color <- #cyan;
				self.party_people  <- 0.0;
				self.chill_people  <- 1.0;
				self.rock_fan_people  <- 0.0;
				self.disco_dancer_people  <- 0.5;
				self.pop_queen_people  <- 0.0;
				self.meat_lover_people <- 1.0;
				self.vegan_people <- 0.0;
			} else if self.type = 'rock_fan' {
				self.color <- #orange;
				self.party_people  <- 0.4;
				self.chill_people  <- 0.6;
				self.rock_fan_people  <- 1.0;
				self.disco_dancer_people  <- 0.2;
				self.pop_queen_people  <- 0.8;
				self.meat_lover_people <- 1.0;
				self.vegan_people <- 0.0;
			} else if self.type = 'disco_dancer' {
				self.color <- #magenta;
				self.party_people  <- 0.8;
				self.chill_people  <- 0.4;
				self.rock_fan_people  <- 0.6;
				self.disco_dancer_people  <- 1.0;
				self.pop_queen_people  <- 0.2;
				self.meat_lover_people <- 0.0;
				self.vegan_people <- 1.0;
			} else if self.type = 'pop_queen' {
				self.color <- #black;
				self.party_people  <- 1.0;
				self.chill_people  <- 0.6;
				self.rock_fan_people  <- 0.2;
				self.disco_dancer_people  <- 0.4;
				self.pop_queen_people  <- 0.0;
				self.meat_lover_people <- 0.0;
				self.vegan_people <- 1.0;
			}else if self.type = 'meat_lover' {
				self.color <- #yellow;
				self.party_people  <- 1.0;
				self.chill_people  <- 0.6;
				self.rock_fan_people  <- 0.2;
				self.disco_dancer_people  <- 0.4;
				self.pop_queen_people  <- 0.0;
				self.meat_lover_people <- 1.0;
				self.vegan_people <- 0.0;
			}else if self.type = 'vegan' {
				self.color <- #violet;
				self.party_people  <- 1.0;
				self.chill_people  <- 0.6;
				self.rock_fan_people  <- 0.2;
				self.disco_dancer_people  <- 0.4;
				self.pop_queen_people  <- 0.0;
				self.meat_lover_people <- 0.0;
				self.vegan_people <- 1.0;
			}
		}
		draw circle(1) color: self.color;
	}
	
	reflex move when: self.targetPoint != nil {
		if (distance_to(self.location, self.targetPoint) > 5#m) {
			do goto target: self.targetPoint;
		}
		do wander;
	}
	
	reflex choosePlace when: (self.time=self.time_limit) or (!self.initialized) {
		self.time <- 0;
		self.time_limit <- rnd(200,400);
		self.initialized <- true;
		self.limit_interactions <- false;
		self.participant <- false;
		self.initiater <- false;
		self.initiater_name<- nil;
		self.guestList<- nil;
		self.dialoge <- false;
		
		if self.type = 'party' {
			self.targetPoint <- placeListDancefloor[rnd(0, length(placeListDancefloor)-1)].location;
		} else if self.type = 'chill' {
			self.targetPoint <- placeListPub[rnd(0, length(placeListPub)-1)].location;
		} else if self.type = 'meat_lover' {
			self.targetPoint <- placeListResturant[rnd(0, length(placeListResturant)-1)].location;
		} else if self.type = 'vegan' {
			self.targetPoint <- placeListResturant[rnd(0, length(placeListResturant)-1)].location;
		} else if self.type = 'rock_fan' {
			self.targetPoint <- placeListRock[rnd(0, length(placeListRock)-1)].location;
		} else if self.type = 'disco_dancer' {
			self.targetPoint <- placeListDancefloor[rnd(0, length(placeListDancefloor)-1)].location;
		} else if self.type = 'pop_queen' {
			self.targetPoint <- placeListPop[rnd(0, length(placeListPop)-1)].location;
		}
	}
	reflex startconversation when: (self.limit_interactions=false) and !self.participant  {
		guestList <- guest at_distance 5;
		
		if !empty(guestList) and !self.participant{
			write  name + ' I would like to know your character';
			//write guestList;
			self.limit_interactions <- true;
			
			do start_conversation with: [to :: guestList, protocol :: 'fipa-request', performative :: 'inform', contents ::  [self.name,self.type] ];
			initiater <- true;
			list lguest <- guestList;
			//int M <- length(guestList2);
			loop i from:0 to:length(lguest)-1 {
				participant_name<- guestList[i];

				participant_name.participant<- true;
				participant_name.limit_interactions<- true;
				add self to: participant_name.initiater_name;

			}
			self.initiater <- true;
			self.participant <- false;
		}
		if !empty(guestList) and self.participant{
			self.dialoge <-true;
			
		}
		 
	}

	
	reflex interact when:  !empty(informs) {
    	message m <- informs[0];
		list<unknown> c <- m.contents;			

			if (c[1] = 'party') {
				write string(c[0]) + 'asks: Hi, ' + string(self.name) + '. I am ' + string(c[1]) + ' and I want to dance with you.';

				if self.sympathetic>=5 {
					decider <- flip(self.party_people);
					if decider=true {
						accept_rate <- accept_rate + 1;
						do agree with: [ message :: m, contents :: ['interested'] ];
						write string(self.name) + 'says: Sure, ' + string(c[0]) + '. I am ' + string(self.type) + ' and I accept to dance with you.';
					} else if decider=false {
						reject_rate <- reject_rate + 1;
						do refuse with: [ message :: m, contents :: ['not-interested'] ];
						write string(self.name) + 'says: No, ' + string(c[0]) + '. I am ' + string(self.type) + ' and I do not like ' + string(c[1]) + ' people.';
					}
				} else if self.sympathetic<5 {
					decider <- false;
					reject_rate <- reject_rate + 1;
					write string(self.name) + 'says: No, ' + string(c[0]) + '. I am sorry, but I am not sympathetic enough to dance with you.';
					do refuse with: [ message :: m, contents :: ['not-interested'] ];
				}
			} else if (c[1] = 'chill') {
				write string(c[0]) + 'asks: Hi, ' + string(self.name) + '. I am ' + string(c[1]) + '. Would you buy me a drink?';
				if self.sympathetic>=5 {
					decider <- flip(self.chill_people );
					if decider=true {
						accept_rate <- accept_rate + 1;
						do agree with: [ message :: m, contents :: ['interested'] ];
						write string(self.name) + 'says: Sure, ' + string(c[0]) + '. I am ' + string(self.type) + ' and I will buy you a drink..';
					} else if decider=false {
						reject_rate <- reject_rate + 1;
						do refuse with: [ message :: m, contents :: ['not-interested'] ];
						write string(self.name) + 'says: No, ' + string(c[0]) + '. I am ' + string(self.type) + ' and I do not like ' + string(c[1]) + ' people.';
					}
				} else if self.sympathetic<5 {
					decider <- false;
					reject_rate <- reject_rate + 1;
					write string(self.name) + 'says: No, ' + string(c[0]) + '. I am sorry, but I am not sympathetic enough to buy you a drink.';
					do refuse with: [ message :: m, contents :: ['not-interested'] ];
				}
			} else if (c[1] = 'rock_fan') {
				write string(c[0]) + 'asks: Hi, ' + string(self.name) + '. I am ' + string(c[1]) + '. Will you go to a rock concert with me?';
				if self.kind>=5 {
					decider <- flip(self.rock_fan_people );
					if decider=true {
						accept_rate <- accept_rate + 1;
						do agree with: [ message :: m, contents :: ['interested'] ];
						write string(self.name) + 'says: Sure, ' + string(c[0]) + '. I am ' + string(self.type) + '. Lets go!';
					} else if decider=false {
						reject_rate <- reject_rate + 1;
						do refuse with: [ message :: m, contents :: ['not-interested'] ];
						write string(self.name) + 'says: No, ' + string(c[0]) + '. I am ' + string(self.type) + ' and I do not like ' + string(c[1]) + ' people.';
						do refuse with: [ message :: m, contents :: ['not-interested'] ];
					}
				} else if self.kind<5 {
					decider <- false;
					reject_rate <- reject_rate + 1;
					write string(self.name) + 'says: No, ' + string(c[0]) + '. I am sorry, but I am not kind enough to go with you.';
					do refuse with: [ message :: m, contents :: ['not-interested'] ];
				}
			} else if (c[1] = 'disco_dancer') {
				write string(c[0]) + 'asks: Hello, ' + string(self.name) + '. I am ' + string(c[1]) + '. Want to dance with me?';
				if self.sympathetic>=5 {
					decider <- flip(self.disco_dancer_people );
					if decider=true {
						accept_rate <- accept_rate + 1;
						do agree with: [ message :: m, contents :: ['interested'] ];
						write string(self.name) + 'says: Sure, ' + string(c[0]) + '. I am ' + string(self.type) + '. Sure, lets go!';
					} else if decider=false {
						reject_rate <- reject_rate + 1;
						do refuse with: [ message :: m, contents :: ['not-interested'] ];
						write string(self.name) + 'says: No, ' + string(c[0]) + '. I am ' + string(self.type) + ' and I do not like ' + string(c[1]) + ' people.';
					}
				} else if self.sympathetic<5 {
					decider <- false;
					reject_rate <- reject_rate + 1;
					write string(self.name) + 'says: No, ' + string(c[0]) + '. I am sorry, but I am not sympathetic enough to dance with you.';
					do refuse with: [ message :: m, contents :: ['not-interested'] ];
				}
			} else if (c[1] = 'pop_queen') {
				write string(c[0]) + 'asks: Hello, ' + string(self.name) + '. I am ' + string(c[1]) + ' and I want to invite you to a pop concert.';
				if self.generous>=5 {
					decider <- flip(self.pop_queen_people );
					if decider=true {
						accept_rate <- accept_rate + 1;
						do agree with: [ message :: m, contents :: ['interested'] ];
						write string(self.name) + 'says: Sure, ' + string(c[0]) + '. I am ' + string(self.type) + ' and I accept your invitation.';
					} else if decider=false {
						reject_rate <- reject_rate + 1;
						do refuse with: [ message :: m, contents :: ['not-interested'] ];
						write string(self.name) + 'says: No, ' + string(c[0]) + '. I am ' + string(self.type) + ' and I do not like ' + string(c[1]) + ' people.';
					}
				} else if self.generous<5 {
					decider <- false;
					reject_rate <- reject_rate + 1;
					write string(self.name) + 'says: No, ' + string(c[0]) + '. I am sorry, but I am not generous enough to accept the invitation.';
					do refuse with: [ message :: m, contents :: ['not-interested'] ];
				}
			}
			
			else if (c[1] = 'meat_lover') {
				write string(c[0]) + 'asks: Hello, ' + string(self.name) + '. I am ' + string(c[1]) + '. Want to have a stake with me?';
				decider <- flip(self.meat_lover_people );
					if decider=true {
						accept_rate <- accept_rate + 1;
						do agree with: [ message :: m, contents :: ['interested'] ];
						write string(self.name) + 'says: Sure, ' + string(c[0]) + '. I am ' + string(self.type) + '. Its my pleasure.';
					} else if decider=false {
						reject_rate <- reject_rate + 1;
						do refuse with: [ message :: m, contents :: ['not-interested'] ];
						write string(self.name) + 'says: No, ' + string(c[0]) + '. I am ' + string(self.type) + ' and I do not like ' + string(c[1]) + ' people.';
					}
			} else if (c[1] = 'vegan') {
				write string(c[0]) + 'asks: Hello, ' + string(self.name) + '. I am ' + string(c[1]) + '. Want to have a meal with me?';
				if self.generous>=5 {
					decider <- flip(self.vegan_people );
					if decider=true {
						accept_rate <- accept_rate + 1;
						do agree with: [ message :: m, contents :: ['interested'] ];
						write string(self.name) + 'says: Sure, ' + string(c[0]) + '. I am ' + string(self.type) + '. Sure, Lets go!';
					} else if decider=false {
						reject_rate <- reject_rate + 1;
						do refuse with: [ message :: m, contents :: ['not-interested'] ];
						write string(self.name) + 'says: No, ' + string(c[0]) + '. I am ' + string(self.type) + ' and I do not like ' + string(c[1]) + ' people.';
					}
				} else if self.generous<5 {
					decider <- false;
					reject_rate <- reject_rate + 1;
					write string(self.name) + 'says: No, ' + string(c[0]) + '. I am sorry, but I am not generous enough to eat with you.';
					do refuse with: [ message :: m, contents :: ['not-interested'] ];
				}
			}
		
		self.limit_interactions <- true;
	}
}

experiment Festival type: gui {
	output {
		display Festival {
			species place aspect: base;
			species guest aspect: base;
		}
		
		display Chart refresh: every(500#cycles) {
			chart "Acceptance and rejection" type: pie { 
				data "Acceptance" value: accept_rate color: #green;
				data "Rejection" value: reject_rate color: #red;
			}
		}
	}
}