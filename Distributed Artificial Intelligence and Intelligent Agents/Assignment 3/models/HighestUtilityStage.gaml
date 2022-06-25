/**
* Name: HighestUtilityStage
* Based on the internal empty template. 
* Author: rafatkhan, ermiashabtegabr
* Tags: 
*/


model HighestUtilityStage

/* Insert your model definition here */

global {
	list<string> festival_acts <- ['concert', 'stand-up', 'auction', 'fireworks'];
	
	init {
		create Stage number: 4 returns: stages;
		create Guest number: 50;
	}
}

species Stage skills:[fipa] {
	rgb clr <- rnd_color(255);
	
	string act;
	
	float lighting;
	float food_near;
	float toilet_near;
	float sitting_places;
	
	bool displaying_act <- false;
	
	int act_timer <- 0;
	int act_duration <- -1;
	
	list<Guest> guests_present_for_act; 

	reflex create_new_act when: !displaying_act {
		int new_act <- rnd(0,3);
		act <- festival_acts at new_act;
		
		act_duration <- rnd(30,70);
		lighting <- rnd(0.1,0.9);
		food_near <- rnd(0.1, 0.9);
		toilet_near <- rnd(0.1, 0.9);
		sitting_places <- rnd(0.1, 0.9);
		displaying_act <- true;
		
		do announce_new_act;
	}
	
	reflex stop_current_act when: act_timer = act_duration {
		displaying_act <- false;
		act_timer <- 0;
		act_duration <- -1;
		do announce_act_finished;
	}
	
	reflex handle_act_timer when: displaying_act {
		act_timer <- act_timer + 1;
	}
	
	action announce_new_act {
		guests_present_for_act <- Guest at_distance(100);
		write '(Time ' + time + '): ' + self.name + ' announcing new act ' + act;
		map<string, float> attribute_values <- map<string, float>("lighting"::lighting, "food_near"::food_near, "toilet_near"::toilet_near, 
		"sitting_places"::sitting_places);
		do start_conversation to: guests_present_for_act protocol: 'fipa-contract-net' performative: 'inform' contents: [attribute_values];
	}
	
	action announce_act_finished {
		write '(Time ' + time + '): ' + self.name + ' announcing act ' + act + ' finished';
		do start_conversation to: guests_present_for_act protocol: 'fipa-contract-net' performative: 'request' contents: ['Act is finished, please leave.'];
		displaying_act <- false;
	}
	
	aspect default {
		draw pyramid(5) at:location color: clr;
	}
}

species Guest skills:[fipa, moving]{
	float pref_sitting_places <- rnd(0.1,0.9);
	float pref_toilet_near <- rnd(0.1,0.9);
	float pref_food_near <- rnd(0.1,0.9);
	float pref_lighting <- rnd(0.1,0.9);
	
	rgb clr <- rnd_color(255);
	
	Stage chosen_stage <- nil;
	
	bool attending_act <- false;
	
	
	reflex wander_around when: chosen_stage = nil and !attending_act{
		do wander;
	}
	
	reflex attend_act when: chosen_stage != nil and self distance_to chosen_stage = 2.0 {
		attending_act <- true;
	}
	
	reflex read_new_act_inform when: !empty(informs) and !attending_act {
		list<Stage> stages <- informs collect (each.sender);
		Stage best_stage <- stages with_max_of (calculate_utility(each));
		chosen_stage <- best_stage;
		
		loop i over: informs {
			do end_conversation message:i contents:['Message from ' + i.sender + ' received.'];
		}
		write '(Time ' + time + '): ' + self.name + ' going to attend an act in ' + best_stage.name + '.';
	}
	
	reflex read_act_finished_inform when: !empty(requests) {
		loop r over: requests {
			Stage s <- r.sender;
			write '(Time ' + time + '): ' + s.name + ' announced the end of act.';
			do agree message:r contents:['Ok, thank you for the show'];
		}
		attending_act <- false;
		chosen_stage <- nil;
	}
	
	float calculate_utility(Stage stg) {
		float utlt <- ((pref_sitting_places * stg.sitting_places) 
			+ (pref_toilet_near * stg.toilet_near) 
			+ (pref_food_near * stg.food_near)
			+ (pref_lighting * stg.lighting)) with_precision 1 ;
		return utlt;
	}
	
	reflex go_to_preferred_stage when: chosen_stage != nil and !attending_act {
		do goto target:chosen_stage;
	}
	
	aspect default {
		draw sphere(1) at:location color: clr;
	}
}

experiment main type: gui {
	output {
		display displayFest type: opengl {
			species Guest;
			species Stage;
		}	
	}
}