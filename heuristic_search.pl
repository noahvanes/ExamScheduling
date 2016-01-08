%%% MODULE HEURISTIC SEARCH %%%

/**
	This module contains the heuristic search strategy that is used to approximate optimal schedules in larger datasets.
	It comes with a variant that is PAC and where a time limit can be set.
*/

:- module(heuristic_search,
	[find_heuristically/1,
	 find_heuristically/2]).


%%% IMPORTS %%%

:- use_module(is_valid, [is_valid/1]).
:- use_module(cost, [exams_cost/2]).
:- use_module(utils, [take/4,min/3]).
:- use_module(exams, [takes_exam/2,conflict/2,schedule/2,room_suitable/2,room_available/5]).

%%% HEURISTIC SEARCH %%%

%% find_heursitically(-Schedule,+T)
%%     Search for an approximately optimal Schedule, 
%%     The algorithm is PAC, and will run for T seconds
%%     In essence it consists of two phases:
%%			I) (greedy) branch-and-bound DFS <- PAC!
%%			II) local beam search <- not PAC, but useful optimization.
%%     Time is split between both phases, so that they each run T/2 seconds
find_heuristically(schedule(S),T):-
	% -- REGISTER DEADLINES -- %
	Period is round(T/2),
	get_time(StartTime),
	Deadline1 is StartTime + Period,
	Deadline2 is Deadline1 + Period,
	% -- GENERATE ONE DUMMY SCHEDULE AS A REFERENCE -- %
	is_valid(schedule(InitEvents)),
	!, %red cut <- only one schedule needed
	schedule(InitExams,InitEvents),
	exams_cost(InitExams,InitCost),
	% -- PHASE 1: (GREEDY) BRANCH-AND-BOUND DFS -- %
	initial_state(Init),
	greedy_search([(Init,0)],Deadline1,InitExams,InitCost,Schedule,Cost),
	% -- PHASE 2: (OPIMIZATION) LOCAL BEAM SEARCH -- %
	min(Period,300,BeamWidth),
	beam_search([(Schedule,Cost)],BeamWidth,Deadline2,ImprovedSchedule),
	% -- OUTPUT IN SCHEDULE FORMAT -- %
	schedule(ImprovedSchedule,S).

%% find_heuristically(-Schedule)
%%     If no time limit is specified, the default is two minutes
find_heuristically(S):-
	find_heuristically(S,120).


%%% (GREEDY) BRANCH-AND-BOUND DFS %%% 

%% greedy_search(+Agenda,+Deadline,+BestSchedule,+BestCost,-Schedule,-Cost)
%%     Agenda is the list of nodes to visit in the order given
%% 	   Deadline is when the search should stop
%%     BestSchedule is the best schedule found so far
%%     BestCost is the best cost encountered so far
%%	   Schedule is the final best schedule that the algorithm finds
%%     Cost is the final best cost that is associated with Schedule 
%%	   => Short summary:
%%			This algorithm does DFS, but sorts the children by their cost to consider the best ones first
%%          Hence, it is a greedy algorithm that will always proceed with the best local successor first
%%			However, since it is DFS it can backtrack, so the search is complete and therefore optimal as T goes to infinite
%%			Since cost is nondecreasing as exams are added, branches whose cost exceeds the current best solution are pruned
greedy_search([],_,X,C,X,C):-
	!. %red cut <- avoid duplicates when checking deadline
greedy_search(_,Deadline,X,C,X,C):-
	get_time(CurrentTime),
	CurrentTime >= Deadline,
	!. %red cut <- stop the search when out of time
greedy_search([(_,SC)|R],Deadline,BestS,BestC,X,C):-
	SC >= BestC, %we can safely prune this branch away
	!, %red cut <- since we prune this branch, do not attempt to expand it
	greedy_search(R,Deadline,BestS,BestC,X,C).
greedy_search([(State,SC)|R],Deadline,_,BestC,X,C):-
	goal(State,SE),
	SC < BestC, %new best solution found!
	!, %red cut <- do not attempt to expand goal states
	greedy_search(R,Deadline,SE,SC,X,C).
greedy_search([(State,_)|R],Deadline,BestS,BestC,X,C):-
	findall((NewState,NewCost),
			(successor(State,NewState),
		 	 node_cost(NewState,NewCost)),
			Children),
	sort(2,@=<,Children,SortedChildren),
	append(SortedChildren,R,NewAgenda),
	!, % green cut <- ensure proper tail call
	greedy_search(NewAgenda,Deadline,BestS,BestC,X,C).


%%% LOCAL BEAM SEARCH %%%

%% beam_search(+Beam,+BeamWidth,+Deadline,-Schedule)
%% 		Beam is the current beam of schedules, sorted by their cost
%%		BeamWidth is the maximum specified width of the search beam
%%		Deadline is when the search should stop
%%		Schedule is the final best schedule found by this algorithm
%%		=> Short summary:
%%			This algorithm can be seen as a variation on beam search/hillclimbing
%%			On each iteration, each schedule in the beam produces one mutation
%%			A maximum of 'BeamWidth' best schedules is always kept in memory
beam_search([(Schedule,_)|_],_,Deadline,Schedule):-
	get_time(CurrentTime),
	CurrentTime >= Deadline,
	!. %red cut <- stop the search when out of time
beam_search(CurrentBeam,N,Deadline,X):-
	findall((NewExams,NewCost),
			(member((Exams,_),CurrentBeam),
			 mutation(Exams,NewExams),
			 !, %red cut <- only add one mutation per schedule
			 exams_cost(NewExams,NewCost)),
			CandidateStates,
			CurrentBeam),
	sort(2,@<,CandidateStates,SortedStates),
	take(SortedStates,N,NewBeam,_),
	!, % green cut <- ensure proper tail call
	beam_search(NewBeam,N,Deadline,X).

%% mutation(+Schedule,-Mutation)
%%     Mutation is a mutation of a given Schedule
%%	   It takes out two events randomly and reschedules them
mutation(Exams,Mutation):-
	random_select(exam(E1,_,_,_,_),Exams,R1),
	random_select(exam(E2,_,_,_,_),R1,R2),
	random_successor(([E1,E2],R2),IntermediateState),
	random_successor(IntermediateState,([],Mutation)).


%%% SEARCH PROBLEM %%%

%% goal(?GoalState,?Schedule)
%%     GoalState is a final/goal state
%%     This is the case when no exams are left to be scheduled
%%     Schedule is the corresponding schedule found
goal(([],S),S).

%% initial_state(-State)
%%     State is the initial state of the search
%%     Optimized so that important exams are first to be scheduled
initial_state((E,[])):-
	findall((E,N),
			(bagof(S,takes_exam(S,E),L),
			 length(L,N)),
			Exams),
	sort(2,@>=,Exams,SortedExams),
	exams(SortedExams,E).

%% node_cost(+Node,-Cost)
%%     Node (in the search tree) has (path) cost Cost
node_cost((_,E),C):-
	exams_cost(E,C).

%% successor(?State,?NextState)
%%     NextState is the successor state of State
%%     It takes an exam from State that is yet to be scheduled
%%     and schedules it in NextState without causing any conflicts
successor(([Exam|R],Exams),(R,[NewEntry|Exams])):-
	room_suitable(Exam,Room),
	room_available(Exam,Room,Day,Hour,End),
	NewEntry = exam(Exam,Room,Day,Hour,End),
	not(conflict(NewEntry,Exams)).

%% random_successor(?State,?NextState)
%%     Identical to successor/2, but randomized to improve exploration
random_successor(([Exam|R],Exams),(R,[NewEntry|Exams])):-
	find_random(room_suitable(Exam,Room)),
	find_random(room_available(Exam,Room,Day,Hour,End)),
	NewEntry = exam(Exam,Room,Day,Hour,End),
	not(conflict(NewEntry,Exams)).


%% HELPER PREDICATES %%%

%% exams(+ExamsWithCost,-ExamsWithoutCost)
%%     Helper predicate for initial_state/1
%%     ExamsWithoutCost strips away cost information from ExamsWithCost
exams([],[]).
exams([(E,_)|T],[E|Z]):-
	exams(T,Z).

%% find_random(?Goal)
%%      Meta-predicate that calls Goal
%%      Results are given back in random order
%%		(used internally to randomize successors)
find_random(X):-
	findall(X,X,Results),
	random_permutation(Results,RandomResults),
	member(X,RandomResults).