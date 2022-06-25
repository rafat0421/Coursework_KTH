/**
* Name: HighestUtilityStageCrowdMass
* Based on the internal empty template. 
* Author: rafatkhan, ermiashabtegabr
* Tags: 
*/


model HighestUtilityStageCrowdMass

/* Insert your model definition here */

global {
	list<string> festival_acts <- ['consert', 'standup', 'auction', 'fireworks'];
	list<Guest> guests_that_like_crowd;
	list<Guest> guests_that_dislike_crowd;
	Leader festival_leader;
	
	init {
		create Stage number: 4;
		create Leader number: 1 returns: leader;
		
		create Guest number: 25 {
			crowd_mass <- 0.0;
		}
		
		create Guest number: 25 {
			crowd_mass <- 1.0;
		}
		
		festival_leader <- leader at 0;
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
		
		act_duration <- rnd(50,70);
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

species Leader skills: [fipa] {
	
	reflex read_utilities_from_guests when: !empty(informs) {
		list<Guest> guest_list <- informs collect (each.sender as Guest);
		list contents_list <- informs collect (each.contents as list);
		map<Stage, int> guests_per_stage <- [];
		
		int counter <- 0;
		loop inform_content over:contents_list {
			float guest_crowd_attr <- inform_content at 0 as float;
			list<Stage> sorted_stage_list <- inform_content at 1 as list<Stage>;

			Stage proposed_stage <- propose_guest_with_a_stage(guests_per_stage, guest_crowd_attr, sorted_stage_list);
			
			if (guests_per_stage[proposed_stage] != nil) {
				guests_per_stage[proposed_stage] <- guests_per_stage[proposed_stage] + 1;
			} else {
				add proposed_stage::0 to:guests_per_stage;
			}
			
			do send_stage_proposal((guest_list at counter), proposed_stage);
			counter <- counter + 1;
		}
	}
	
	action send_stage_proposal(Guest guest, Stage stage) {
		do start_conversation to: [guest] protocol: 'fipa-contract-net' performative: 'propose' contents: [stage]; 
	}
	
	Stage propose_guest_with_a_stage(map<Stage, int> guests_per_stage, float guest_crowd_attr, list<Stage> sorted_stage_list) {
		if (guest_crowd_attr = 0.0) {
			return sorted_stage_list at 0;
		}
		
		
		loop stage over: sorted_stage_list {	
			if (guest_crowd_attr = 1.0 and (guests_per_stage[stage] = nil or guests_per_stage[stage] <= 2)) {
				return stage;
			}
		}
		
		// If no good stage found, the first stage in the sorted list is returned 
		return sorted_stage_list at 0;
	}
}

species Guest skills:[fipa, moving]{
	list<Stage> stages_sorted_based_on_utility;
	float pref_sitting_places <- rnd(0.0,1.0);
	float pref_toilet_near <- rnd(0.0,1.0);
	float pref_food_near <- rnd(0.0,1.0);
	float pref_lighting <- rnd(0.0,1.0);
	float crowd_mass;
	
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
		list<Stage> stages;
		
		if length(stages_sorted_based_on_utility) = 0 {
			stages <- informs collect (each.sender);
		} else {
			remove from:stages_sorted_based_on_utility index:0;
			stages <- stages + stages_sorted_based_on_utility;
		}
		
		stages_sorted_based_on_utility <- stages sort_by (calculate_utility(each));
		Stage best_stage <- stages_sorted_based_on_utility at 0;
		
		write '(Time ' + time + '): ' + best_stage.name + ' found stage with best utility.';
		
		loop i over: informs {
			do end_conversation message:i contents:['Message from ' + i.sender + ' received.'];
		}

		write '(Time ' + time + '): ' + self.name + ' informing leader about preferred stage.';
		do start_conversation to: [festival_leader] protocol: 'fipa-contract-net' performative: 'inform' contents: [crowd_mass, stages_sorted_based_on_utility]; 
	}
	
	reflex read_leader_proposal when: !empty(proposes) {
		write '(Time ' + time + '): ' + self.name + ' received proposal from leader.';
		message leader_proposal <- proposes at 0;
		list content <- leader_proposal.contents;
		chosen_stage <- content at 0 as Stage;
		write '(Time ' + time + '): leader proposed ' + chosen_stage.name + '.';
		do end_conversation message:leader_proposal contents:['Proposal received'];
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
