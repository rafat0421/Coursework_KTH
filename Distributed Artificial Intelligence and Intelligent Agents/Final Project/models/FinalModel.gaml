/**
* Name: NewModel
* Based on the internal empty template. 
* Author: rafat
* Tags: 
*/


model FinalModel

/* Insert your model definition here */

global {
	
	string DRINK_OFFER <- "Beer on me!";
	string ACCEPT_DRINK <- "OK! Let's have one!";
	string DECLINE_DRINK <- "Unfortunately, I have to work!";
	
	string ORDER_FOOD <- "I would like to order!";
	string FOOD_READY <- "The food you ordered is ready!";
	
	string DANCE_TOGETHER <- "Lets dance!";
	string ACCEPT_TO_DANCE <- "Yes, lets dance!";
	string REJECT_TO_DANCE <- "Nahhh, I am fine!";
	
	string WANT_TO_CHAT <- "Hi, do you want to chat?";
	string NO_THANKS <- "No thanks!";
	
	string ROCK_FAN <- 'rock fan';
	string RANDB_FAN <- 'r&b fan';
	string PARTY_GUEST <- 'party person';
	string CHILL_GUEST <- 'chill person';
	string MEAT_LOVER <- 'meat lover';
	string VEGAN <- 'vegan';
	string SECURITY_GUARD <- 'security guard';
	
	string PUB <- 'pub';
	string STAGE <- 'stage';
	string RESTAURANT <- 'restaurant';
	string WC <- 'wc';
	string ATM <- 'atm';
	
	string ROCK <- 'rock';
	string RANDB <- 'r&b';
	string HIPHOP <- 'hiphop';
	
	string FRIED_CHICKEN <- 'chicken';
	string BEEF <- 'beef';
	string FISH_AND_CHIPS <- 'fish';
	
	string TOFU_BURGER <- 'tofu burger';
	string VEGAN_PIZZA <- 'vegan pizza';
	
	float BEER_PRICE <- 50.0 with_precision 2;
	int VIEW_DISTANCE <- 10;
	
	list<string> guest_type <- [ROCK_FAN, RANDB_FAN, PARTY_GUEST, CHILL_GUEST, MEAT_LOVER, VEGAN, SECURITY_GUARD];
	list<string> place_type <- [PUB, STAGE, RESTAURANT, WC, ATM];
	list<string> music_list <- [ROCK, RANDB, HIPHOP];
	list<string> food_list <- [FRIED_CHICKEN, BEEF, FISH_AND_CHIPS, TOFU_BURGER, VEGAN_PIZZA];
	list<string> non_vegan_food_list <- [FRIED_CHICKEN, BEEF, FISH_AND_CHIPS];
	list<string> vegan_food_list <- [TOFU_BURGER, VEGAN_PIZZA];
	
	int accept_rate <- 0;
	int reject_rate <- 0;
	float total_happiness <- 0;
	float happiness_multiplier <- 100;
	float total_money <- 0;
	
	init {
		create RockFan number: 8;
		create RandBFan number: 8;
		create PartyGuest number: 10;
		create ChillGuest number: 8;
		create MeatLover number: 8;
		create Vegan number: 6;
		
		create SecurityGuard number: 2;
		
		create Stage number: 1;
		create Pub number: 1;
		create Restaurant number: 1;
		create Wc number: 1;
		create atm number: 1;
	}
}


species Guest skills: [moving, fipa] {
	rgb guest_color;
	
	int hunger <- rnd(20,50) min: 0 update: hunger - 1;
	int visit_restroom <- rnd(20,30) min: 0 update: visit_restroom - 1;
	 
	int time_passed;
	int time_in_current_place;
	int number_of_times_reported;
	
	float drunkness;
	float loudness;
	float generous;
	float budget;
	
	bool talkative;
	bool just_arrived;
	bool drinks_alcohol;
	bool can_afford_more_food;
	
	bool is_at_stage;
	bool is_at_pub;
	bool is_at_restaurant;
	bool is_at_wc;
	bool is_at_atm;
	
	Wc current_wc;
	Pub current_pub;
	Stage current_stage;
	Restaurant current_restaurant;
	
	string dest;
	point next_place;
	bool already_served;
	bool has_money;
	string type_of_guest;
	string preferred_genre;
	Guest guest_speaking_with;
	
	init {
		guest_color <- #lightgreen;
		number_of_times_reported <- 0;
		talkative <- flip(0.5);	
		drinks_alcohol <- flip(0.5);
		time_passed <- rnd(20,30);
		time_in_current_place <- 0;
		loudness <- rnd(0.0,1.0) with_precision 2;
		generous <- rnd(0,1) with_precision 2;
		budget <- rnd(500.0,700.0) with_precision 2;
		preferred_genre <- music_list[rnd(2)];
		drunkness <- 0.0 with_precision 2;
		
		just_arrived <- true;
		already_served <- false;
		has_money <- true;
		can_afford_more_food <- true;
		
		is_at_stage <- false;
		is_at_pub <- false;
		is_at_restaurant <- false;
		is_at_wc <- false;
		is_at_atm <- false;
		
		guest_speaking_with <- nil;
		total_money <- total_money + budget;
	}	
	
	aspect info {
		draw circle(1) at: location color: guest_color border: #black;
		draw self.name size: 3 color: #black;
	}
	
//	reflex write_debug {
//		write "----------------------------------------------";
//		write "Name: " + self.name;
//		write "Time passed: " + self.time_passed + ", time in place: " + self.time_in_current_place;
//		write "Loudness: " + self.loudness + ", generous: " + self.generous + ", budget: " + self.budget;
//		write "Preferred genre: " + self.preferred_genre + ", drunkness: " + self.drunkness;
//		write "----------------------------------------------";
//	}
	
	reflex set_next_place when: (time_passed = time_in_current_place) or just_arrived {
		just_arrived <- false;
		time_passed <- rnd(10,20);
		time_in_current_place <- 0;
		
		write "(Time " + time + ") : " + self.name + " setting new place!";
		
		if (self.budget < 100) {
			atm closes_atm <- atm closest_to(self.location);
			next_place <- closes_atm.location;
			dest <- ATM;
			total_happiness <- total_happiness - happiness_multiplier;
		}
		else if (hunger <= 0 and can_afford_more_food) {
			write "(Time " + time + ") : " + self.name + " is hungry and want to eat!";
			Restaurant closes_restaurant <- Restaurant closest_to(self.location);
			next_place <- closes_restaurant.location;
			current_restaurant <- next_place as Restaurant;
			dest <- RESTAURANT;
			already_served <- false;
		} else if (visit_restroom <= 0) {
			write "(Time " + time + ") : " + self.name + " needs to visit the rest room!";
			Wc closest_wc <- Wc closest_to(self.location);
			next_place <- closest_wc.location;
			current_wc <- next_place as Wc;
			dest <- WC;
		} else if (drinks_alcohol and (drunkness < 0.8) and (budget > 2*BEER_PRICE)) {
			write "(Time " + time + ") : " + self.name + " want to drink beer!";
			Pub closest_pub <- Pub closest_to(self.location);
			next_place <- closest_pub.location;
			current_pub <- next_place as Pub;
			dest <- PUB;
		} else if (flip(0.2)) {
			write "(Time " + time + ") : " + self.name + " want to go to a stage!";
			Stage closest_stage <- Stage closest_to(self.location);
			next_place <- closest_stage.location;
			current_stage <- next_place as Stage;
			dest <- STAGE;
			if (closest_stage.music_genre != preferred_genre) {
				time_in_current_place <- time_passed / 2 as int;	
			} 
		} else {
			next_place <- nil;
		}
		
		do reset_place_variables;
	}
	
	reflex wander_around when: next_place = nil {
		self.time_in_current_place <- self.time_in_current_place + 1;
		do wander;
	}
	
	reflex move_to_target when: next_place != nil {
		if (distance_to(self.location, next_place) > 5#m) {
			do goto target: next_place;
		} else {
			is_at_restaurant <- dest = RESTAURANT;
			is_at_pub <- dest = PUB;
			is_at_wc <- dest = WC;
			is_at_atm <- dest = ATM;
			is_at_stage <- dest = STAGE;
			next_place <- {0,0};
		}
	}
	
	reflex report_bad_guest when: find_bad_guest() != nil{
		Guest bad_guest <- find_bad_guest();
		if (bad_guest != nil) {
			write "(Time " + time + ") : " + self.name + " is reporting " + bad_guest.name;
			ask bad_guest {
				self.number_of_times_reported <- self.number_of_times_reported + 1;
			}
			
			SecurityGuard closest_guard <- SecurityGuard closest_to(self.location);
			do goto target: closest_guard;
			
			if (closest_guard != nil and self distance_to(closest_guard) < 3) {
				write "(Time " + time + ") : " + self.name + " is reporting " + bad_guest.name + " to " + closest_guard.name;
				ask closest_guard {
					do throw_out_bad_guest(bad_guest);
					total_happiness <- total_happiness + happiness_multiplier;
				}
			} 
		} 
	}
	
	reflex handle_interaction_in_pub when: is_at_pub {
		self.time_in_current_place <- self.time_in_current_place + 1;
		write "(Time " + time + ") : " + self.name + " is in the pub! ";
		list<Guest> guests_in_pub <- Guest at_distance(5);
		if (!self.talkative or empty(guests_in_pub) and can_buy_another_drink()) {
			do drink_beer_alone;
		} else if (self.talkative and want_to_offer_a_drink()) {
			Guest cool_guest <- guest_easy_to_interract_with(guests_in_pub);
			if (cool_guest != nil) {
				do offer_cool_guest_drink_at_bar(cool_guest);	
			} else {
				do drink_beer_alone;
			}
		}
		
		if(!empty(informs)){
			loop offer over: informs {
			Guest sender <- offer.sender as Guest;
			list<string> msg_content <- offer.contents;
			if (msg_content[0] = "BAR" and msg_content[1] = DRINK_OFFER and guest_has_good_music_taste(sender)) {
				do drink_beer_with_guest();
				do accept_proposal message: offer contents: ["BAR", ACCEPT_DRINK];
				accept_rate <- accept_rate + 1;
				total_happiness <- total_happiness + happiness_multiplier;
			} else {
				do reject_proposal message: offer contents: ["BAR", DECLINE_DRINK];
				reject_rate <- reject_rate + 1;
				total_happiness <- total_happiness - happiness_multiplier;
			}
		}
		}
		
		loop accepted over: accept_proposals {
			Guest sender <- accepted.sender as Guest;
			list<string> msg_content <- accepted.contents;
			if (sender = guest_speaking_with and msg_content[0] = "BAR" and msg_content[1] = ACCEPT_DRINK) {
				do buy_drink_for_guest_and_drink_together;
			}
		}
		
		loop rejected over: reject_proposals {
			Guest sender <- rejected.sender as Guest;
			list<string> msg_content <- rejected.contents;
			if (sender = guest_speaking_with and msg_content[0] = "BAR" and msg_content[1] = DECLINE_DRINK) {
				do drink_beer_alone;
			}
		}
	}
		
	reflex handle_interaction_in_restaurant when: is_at_restaurant {
		self.time_in_current_place <- self.time_in_current_place + 1;
		write "(Time " + time + ") : " + self.name + " is in restaurant!";
		if (!already_served) {
			float food_price;
			string preferred_food <- get_preferred_food();
			ask current_restaurant {
				food_price <- get_food_price(preferred_food); 
			}
			
			if ((self.budget - food_price) > 0) {
				self.budget <- self.budget - food_price with_precision 2;
				total_money <- total_money - food_price with_precision 2;
				write "(Time " + time + ") : " + self.name + " ordering food.";
				//total_happiness <- total_happiness + happiness_multiplier;
				do start_conversation 
					to: [current_restaurant] 
					protocol: 'fipa-request' 
					performative: 'inform' 
					contents:["RESTAURANT", ORDER_FOOD, preferred_food];
			} else {
				//Can't afford to buy more food and therefore just leave
				can_afford_more_food <- false;
				self.time_in_current_place <- self.time_passed - 1;
				//total_happiness <- total_happiness - happiness_multiplier;
			}
		} 
		
		loop i over: informs {
			list<string> msg_content <- i.contents;
			if (msg_content[0] = "RESTAURANT" and msg_content[1] = FOOD_READY) {
				write "(Time " + time + ") : " + self.name + " received order."; 
				already_served <- true;
				hunger <- rnd(50,100);
				//total_happiness <- total_happiness + happiness_multiplier;
			}
		}
	} 
	
	reflex handle_interaction_in_stage when: is_at_stage {
		self.time_in_current_place <- self.time_in_current_place + 1;
		write "(Time " + time + ") : " + self.name + " is at a stage!";
		list<Guest> closest_guests <- Guest at_distance(5);
		Guest new_cool_guest <- guest_easy_to_interract_with(closest_guests);
		if (talkative and new_cool_guest != nil) {
			guest_speaking_with <- new_cool_guest;
			do start_conversation 
				to:[new_cool_guest] 
				protocol:'fipa-request' 
				performative:'inform' 
				contents:["STAGE", DANCE_TOGETHER];
				total_happiness <- total_happiness + happiness_multiplier;
		} else {
			do dance_alone;
			total_happiness <- total_happiness - happiness_multiplier;
		}
		
		if (!empty(informs)) {
			if(Guest != nil){
				loop i over: informs {
			Guest guest_asking_to_dance <- i.sender as Guest;
			//if(Guest != nil){
			//	write "(Time " + time + ") : " + self.name + " received offer to dance from " + guest_asking_to_dance.name + ".";
			//}
			if (guest_is_of_type_easy_to_interract_with(guest_asking_to_dance)) {
				do accept_proposal message: i contents: ["STAGE", ACCEPT_TO_DANCE];
				write "(Time " + time + ") : " + self.name + " accepted the proposal";
				accept_rate <- accept_rate + 1;
				total_happiness <- total_happiness + happiness_multiplier;
			} else {
				do reject_proposal message: i contents: ["STAGE", REJECT_TO_DANCE];
				write "(Time " + time + ") : " + self.name + " reject the proposal";
				reject_rate <- reject_rate + 1;
				total_happiness <- total_happiness - happiness_multiplier;
 			}
			}
			}	
		}
		
		loop accepted over: accept_proposals {
			Guest guest_accepted <- accepted.sender as Guest;
			if (guest_accepted = guest_speaking_with) {
				write "(Time " + time + ") : " + self.name + " is dancing with " + guest_accepted.name + ".";
			}
		}
		
		loop rejected over: reject_proposals {
			Guest guest_rejected <- rejected.sender as Guest;
			if (guest_rejected = guest_speaking_with) {
				write "(Time " + time + ") : " + guest_rejected.name + " declined to dance with " + self.name + " :( .";
			}
		}
	}
	
	reflex go_to_atm when: is_at_atm {
		write "(Time " + time + ") : " + self.name + " is using the atm! ";
		self.budget <- self.budget + 500;
		total_money <- total_money + 500;
		has_money <- true;
		self.time_in_current_place <- self.time_in_current_place + 1;
		total_happiness <- total_happiness + happiness_multiplier;
		//next_place <- nil;
	}
	
	reflex go_to_restroom when: is_at_wc {
		write "(Time " + time + ") : " + self.name + " is using the restroom! ";
		visit_restroom <- rnd(30,60);
		drunkness <- (drunkness - 0.2) <= 0 ? 0: drunkness - 0.2 with_precision 2;
		self.time_in_current_place <- self.time_in_current_place + 1;
		//total_happiness <- total_happiness + happiness_multiplier;
	}
	
	Guest find_bad_guest { 
		list<Guest> guest_list <- RockFan at_distance(VIEW_DISTANCE);
		add all: RandBFan at_distance(VIEW_DISTANCE) to: guest_list;
		add all: ChillGuest at_distance(VIEW_DISTANCE) to: guest_list;
		add all: PartyGuest at_distance(VIEW_DISTANCE) to: guest_list;
		add all: MeatLover at_distance(VIEW_DISTANCE) to: guest_list;
		add all: Vegan at_distance(VIEW_DISTANCE) to: guest_list;
		if (!empty(guest_list)) {
			list<Guest> bad_guests <- guest_list where (is_bad_guest(each));
			if (!empty(bad_guests)) {
				return bad_guests[0];
			}
		}
		return nil;
	}
	
	Guest guest_easy_to_interract_with(list<Guest> guests) {
		Guest can_interract_with_guest <- guests first_with (type_of(each) = type_of(self));
		if (can_interract_with_guest != nil) {
			return can_interract_with_guest;
		}

		Guest cool_guest <- guests first_with (guest_is_of_type_easy_to_interract_with(each));
		if (cool_guest != nil) {
			return cool_guest;
		}
		return nil;
	}
	
	bool is_bad_guest(Guest guest) {
		return (guest != nil) and (guest.drinks_alcohol and guest.drunkness > 0.8 and guest.loudness > 0.8);
	}
	
	bool can_buy_another_drink {
		return drinks_alcohol and (budget > 2*BEER_PRICE) and (drunkness < 0.8) and (time_in_current_place > time_passed/2 as int);
	}
	
	bool want_to_offer_a_drink {
		return (drinks_alcohol and (budget > (3*BEER_PRICE)) and (generous > 0.8));
	}
	
	action drink_beer_alone {
		self.drunkness <- self.drunkness + 0.2 with_precision 2;
		self.loudness <- self.loudness + 0.1 with_precision 2;
		self.budget <- self.budget - BEER_PRICE with_precision 2; 
		total_money <- total_money - BEER_PRICE with_precision 2;
		self.visit_restroom <- ((self.visit_restroom - 4) < 0) ? 0 : self.visit_restroom - 4;
	}	
	
	action drink_beer_with_guest {
		self.drunkness <- self.drunkness + 0.2 with_precision 2;
		self.loudness <- self.loudness + 0.1 with_precision 2;
		self.visit_restroom <- ((self.visit_restroom - 4) < 0) ? 0 : self.visit_restroom - 4;
	}
	
	action buy_drink_for_guest_and_drink_together{
		self.drunkness <- self.drunkness + 0.2 with_precision 2;
		self.loudness <- self.loudness + 0.1 with_precision 2;
		self.budget <- self.budget - 2*BEER_PRICE with_precision 2;
		total_money <- total_money - 2*BEER_PRICE with_precision 2;
		self.visit_restroom <- ((self.visit_restroom - 4) < 0) ? 0 : self.visit_restroom - 4;
	}
	
	action dance_alone {
		self.drunkness <- (self.drunkness - 0.2) <= 0 ? 0.0 : self.drunkness - 0.2 with_precision 2;
		self.loudness <- self.loudness + 0.1 with_precision 2;
		self.hunger <- self.hunger - 1;
	}
	
	action offer_cool_guest_drink_at_bar(Guest guest) {
		do start_conversation 
			to:[guest] 
			protocol: 'fipa-protocol' 
			performative: 'inform' 
			contents: ["BAR", DRINK_OFFER];
	}
	
	action reset_place_variables {
		is_at_stage <- false;
		is_at_wc <- false;
		is_at_restaurant <- false;
		is_at_pub <- false;
		is_at_atm <- false;
	}
	
	bool guest_is_of_type_easy_to_interract_with(Guest guest) {
		return false;
	}
	
	bool guest_has_good_music_taste(Guest guest) {
		return guest.preferred_genre = self.preferred_genre;
	}
	
	string get_preferred_food {
		if (self.type_of_guest = MEAT_LOVER) {
			return non_vegan_food_list[rnd(0,length(non_vegan_food_list) - 1)];
		} else if (self.type_of_guest = VEGAN) {
			return vegan_food_list[rnd(0,length(vegan_food_list) - 1)];
		}
		return food_list[rnd(0,length(food_list) - 1)];
	}
}


species RockFan parent: Guest {
	rgb rock_fan_color <- #red;
	
	init {
		preferred_genre <- ROCK;
		type_of_guest <- ROCK;
	}
	
	aspect info {
		draw circle(1) at: location color: rock_fan_color border: #black;
		draw self.name size: 3 color: #black;
	}
	
	bool guest_is_of_type_easy_to_interract_with(Guest guest) {
		if(guest = nil){
			return false;
		}
		string new_guest_type <- guest.type_of_guest;
		return (new_guest_type = self.type_of_guest)
			or (new_guest_type = PARTY_GUEST) 
			or (new_guest_type = MEAT_LOVER) 
			or (new_guest_type = VEGAN) 
			or (guest.preferred_genre = self.preferred_genre);
	}
}


species RandBFan parent: Guest {
	rgb randb_fan_color <- #brown;
	
	init {
		preferred_genre <- RANDB;
		type_of_guest <- RANDB_FAN;
	}
	
	aspect info {
		draw circle(1) at: location color: randb_fan_color border: #black;
		draw self.name size: 3 color: #black;
	}
	
	bool guest_is_of_type_easy_to_interract_with(Guest guest) {
		if(guest = nil){
			return false;
		}
		string new_guest_type <- guest.type_of_guest;
		return (new_guest_type = self.type_of_guest)
			or (new_guest_type = PARTY_GUEST) 
			or (new_guest_type = MEAT_LOVER) 
			or (new_guest_type = VEGAN) 
			or (guest.preferred_genre = self.preferred_genre);
	}
}


species ChillGuest parent: Guest {
	rgb chill_guest_color <- #green;
	
	init {
		preferred_genre <- flip(0.666) ? RANDB : music_list[rnd_choice([0,2])];
		type_of_guest <- CHILL_GUEST;
	}
	
	aspect info {
		draw circle(1) at: location color: chill_guest_color border: #black;
		draw self.name size: 3 color: #black;
	}
	
	bool guest_is_of_type_easy_to_interract_with(Guest guest) {
		if(guest = nil){
			return false;
		}
		string new_guest_type <- guest.type_of_guest;
		return (new_guest_type = self.type_of_guest)
			or (new_guest_type = RANDB_FAN)
			or (new_guest_type = MEAT_LOVER) 
			or (new_guest_type = VEGAN) 
			or (guest.preferred_genre = self.preferred_genre);
	}
}


species PartyGuest parent: Guest {
	rgb party_guest_color <- #pink;
	
	init {
		preferred_genre <- flip(0.666) ? HIPHOP : music_list[rnd_choice([0,1])];
		type_of_guest <- PARTY_GUEST;
	}
	
	aspect info {
		draw circle(1) at: location color: party_guest_color border: #black;
		draw self.name size: 3 color: #black;
	}
	
	bool guest_is_of_type_easy_to_interract_with(Guest guest) {
		if(guest = nil){
			return false;
		}
		string new_guest_type <- guest.type_of_guest;
		return (new_guest_type = self.type_of_guest)
			or (new_guest_type = ROCK_FAN)
			or (new_guest_type = RANDB_FAN)
			or (new_guest_type = PARTY_GUEST) 
			or (new_guest_type = MEAT_LOVER) 
			or (new_guest_type = VEGAN) 
			or (guest.preferred_genre = self.preferred_genre);
	}
}


species MeatLover parent: Guest {
	
	init {
		preferred_genre <- music_list[rnd_choice([0,1,2])];
		type_of_guest <- MEAT_LOVER;
	}
	
	bool guest_is_of_type_easy_to_interract_with(Guest guest) {
		if(guest = nil){
			return false;
		}
		string new_guest_type <- guest.type_of_guest;
		return (new_guest_type = self.type_of_guest)
			or (new_guest_type = ROCK_FAN)
			or (new_guest_type = RANDB_FAN)
			or (new_guest_type = PARTY_GUEST) 
			or (new_guest_type = CHILL_GUEST) 
			or (guest.preferred_genre = self.preferred_genre);
	}
}


species Vegan parent: Guest {

	init {
		preferred_genre <- music_list[rnd_choice([0,1,2])];
		type_of_guest <- VEGAN;
	}
	
	bool guest_is_of_type_easy_to_interract_with(Guest guest) {
		if(guest = nil){
			return false;
		}
		string new_guest_type <- guest.type_of_guest;
		return (new_guest_type = self.type_of_guest)
			or (new_guest_type = ROCK_FAN)
			or (new_guest_type = RANDB_FAN)
			or (new_guest_type = PARTY_GUEST) 
			or (new_guest_type = CHILL_GUEST) 
			or (guest.preferred_genre = self.preferred_genre);
	}
}


species SecurityGuard skills: [moving, fipa]{
	init {
		speed <- 10.0 #km/#h;
	}
	
	rgb guard_color <- #black;
	Guest captured_guest <- nil;
		
	aspect info {
		draw square(3) at: location color: guard_color;
		draw string(captured_guest.name) size: 3 color: #black;
	}
	
	action throw_out_bad_guest(Guest bad_guest) {
		if (bad_guest.number_of_times_reported > 3) {
			ask bad_guest {
				do die;
			}	
		}
	}
}


species Pub {
	rgb pub_color <- #blue;
	int beer <- 20 min: 0;
	
	aspect info {
		draw square(5) at: location color: pub_color;
		draw string("Beers: " + beer) size: 3 color: pub_color;
	}
}


species Restaurant skills: [fipa]{
	rgb restaurant_color <- #brown;
	bool serves_vegan_food;
	map<string, float> menu;
	map<string, Guest> orders_received;
	bool order_received;
	int serving_time;
	float profit;
	
	init {
		orders_received <- [];
		profit <- 0.0 with_precision 2;
		serving_time <- 0;
		menu <- get_menu();
		order_received <- false;
		serves_vegan_food <- flip(0.5);
	}
	
	aspect info {
		draw square(5) at: location color: restaurant_color;
	}
	
	map<string, float> get_menu {
		map<string, float> food_menu;
		
		loop dish over: food_list {
			add rnd(30,40) at: dish to: food_menu;
		}
		return food_menu;
	}
	
	float get_food_price(string dish) {
		return menu[dish];
	}
	
	reflex get_customer_order when: !empty(informs) {
		loop i over: informs {
			Guest customer <- i.sender;
			list<string> order <- i.contents;
			if (order[0] = "RESTAURANT" and order[1] = ORDER_FOOD) {
				write "(Time " + time + ") : Restaurant received order received from " + customer.name;
				add customer at: order[2] to: orders_received;
			}
		}
	}
	
	reflex serve_order when: !empty(orders_received){
		Guest customer <- first (orders_received);
		write "(Time " + time + ") : Restaurant serving " + customer.name;
		remove customer from: orders_received;
		do start_conversation 
			to:[customer] 
			protocol: 'fipa-request' 
			performative: 'inform' 
			contents: ["RESTAURANT", FOOD_READY];
	}
}


species Stage {
	rgb stage_color <- #green;
	string music_genre <- music_list[rnd(2)];
	
	aspect info {
		draw square(5) at: location color: stage_color;
		draw string("Music type: " + music_genre) size: 3 color: stage_color;
	}
}


species Wc {
	rgb wc_color <- #lightblue;

	aspect info {
		draw square(5) at: location color: wc_color;
		draw "WC" size: 3 color: wc_color;
	}
}

species atm skills: [fipa] {
	rgb atm_color <- #red;
	//state 0 reply the guests with the location information with FIPA
	reflex provideLocation when: !empty(cfps) {
        write '(Time ' + time + '): ' + name + ' provide the atm location to guests who need to withdraw money.';
        
		loop c over: cfps {  //c = the message from one of requesting guests
			do propose (message: c, contents:[location]);
		}
	}
	//draw atm
	aspect default {
		draw square(5) color: atm_color;
		draw "atm" size: 3 color: atm_color;
	}
}



experiment Festival type: gui {
	output {
		display info_display {
			species RockFan aspect: info;
			species RandBFan aspect: info;
			species PartyGuest aspect: info;
			species ChillGuest aspect: info;
			species MeatLover aspect: info;
			species Vegan aspect: info;
			species SecurityGuard aspect: info;
			
			species Stage aspect: info;
			species Pub aspect: info;
			species Wc aspect: info;
			species Restaurant aspect: info;
			species atm;
        }
        
        display Chart refresh: every(500#cycles) {
			chart "Acceptance and rejection" type: pie { 
				data "Acceptance" value: accept_rate color: #green;
				data "Rejection" value: reject_rate color: #red;
			}
		}
		
		display chart
		{
			chart "Money and Happiness"
			{
				data "Total Money" value: total_money color: #red;
				data "Total Happiness" value: total_happiness color: #green;
			}
		}
	}
}