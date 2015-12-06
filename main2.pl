:- dynamic c_event/4.

:- use_module(dataset_large).
:- use_module(kb_assertions).
:- use_module(utils).

%:- use_module(library(sort)).


%%% LAST TRY

goal((0,_,_)).

initial_state((N,E,[])):-
	exams(E),
	length(E,N).

state_cost((N,_,E),C):-
	exams_cost(E,C).

successor(([Exam|R],Exams),(R,[NewEntry|Exams])):-
	room_suitable(Exam,Room),
	room_available(Exam,Room,Day,Hour,End),
	NewEntry = exam(Exam,Room,Day,Hour,End),
	not(conflict(NewEntry,Exams)).

find(schedule(S),T):-
	% register deadline
	get_time(StartTime),
	Deadline is StartTime + T,
	% we need a dummy schedule to compare to
	is_valid(schedule(InitEvents)),
	!, % only one schedule needed
	schedule(InitExams,InitEvents),
	exams_cost(InitExams,InitCost),
	% start the search
	initial_state(I),
	search([(I,0)],Deadline,InitExams,InitCost,X),
	% output in desired format
	schedule(X,S).


%search(_,Deadline,X,_,X):-
%	get_time(CurrentTime),
%	CurrentTime >= Deadline,
%	!. %stop the search ...
search([(State,C)|R],Deadline,_,BestC,X):-
	goal(State),
	C < BestC,
	!,
	write('new optimal solution: '), write(C), nl,
	search(R,Deadline,State,C,X).
search([(_,C)|R],Deadline,BestS,BestC,X):-
	C >= BestC,
	!,
	search(R,Deadline,BestS,BestC,X).
search([(State,_)|R],Deadline,BestS,BestC,X):-
	findall((NewState,NewCost),
			(successor(State,NewState),
		 	 state_cost(NewState,NewCost)),
			Children),
	sort(2,@=<,Children,SortedChildren),
	append(SortedChildren,R,NewAgenda),
	!, % tail call optimization
	search(NewAgenda,Deadline,BestS,BestC,X).
search([],_,X,_,X). % unlikely, but possible for small datasets


%%% EXAM PROPERTIES

exam_lecturer(Exam,Lecturer):-
	nonvar(Exam) -> %optimization!
		(has_exam(Course,Exam),
		 teaches(Lecturer,Course));
		(teaches(Lecturer,Course),
		 has_exam(Course,Exam)).

takes_exam(Student,Exam):-
	nonvar(Exam) -> %optimization!
		(has_exam(Course,Exam),
		 follows(Student,Course));
		(follows(Student,Course),
		 has_exam(Course,Exam)).

room_suitable(Exam,Room):-
	required_capacity(Exam,ReqCapacity),
	capacity(Room,Capacity),
	ReqCapacity =< Capacity.

room_available(Exam,Room,Day,Hour,End):-
	duration(Exam,Duration),
	availability(Room,Day,Start,Stop),
	LatestStart is Stop - Duration,
	between(Start,LatestStart,Hour),
	End is Hour + Duration.

takes_exams(S,E1,E2):-
	takes_exam(S,E1),
	takes_exam(S,E2),
	E1 \= E2.



%%% IS_VALID

%conflicts can only occur on the same Day
conflict(exam(E1,R1,D,H1,F1),[exam(E2,R2,D,H2,F2)|_]):-
	overlap(H1,F1,H2,F2), 	%do the scheduled hours of both exams overlap?
	problem(E1,R1,E2,R2). 	%is it a problem to hold both exams concurrently?
conflict(NewExam,[_|Exams]):-
	conflict(NewExam,Exams).

problem(_,R,_,R).		%two overlapping exams in the same room
problem(E1,_,E2,_):-	%two overlapping exams with overlapping participants
	mutual_exclusive(E1,E2).

mutual_exclusive(E1,E2):-
	exam_lecturer(E1,L),
	exam_lecturer(E2,L).
mutual_exclusive(E1,E2):-
	share_students(E1,E2,_).

is_valid(schedule(EventList)):-
	setup_assertions,
	exams(FullExamList),
	is_valid(EventList,FullExamList,[]).

is_valid([],[],_).
is_valid([event(Exam,Room,Day,Hour)|Events],Exams,Reservations):-
	remove_one(Exams,Exam,Remaining),
	!, %red cut <- consider only one exam each time
	room_suitable(Exam,Room),
	room_available(Exam,Room,Day,Hour,End),
	NewEntry = exam(Exam,Room,Day,Hour,End),
	not(conflict(NewEntry,Reservations)),
	is_valid(Events,Remaining,[NewEntry|Reservations]).


%%% EXTRA: repairing broken schedules after mutation/cross-over %%%

random_result(X):-
	findall(X,X,Results),
	random_permutation(Results,RandomResults),
	member(X,RandomResults).

check([],[],[]).
check([event(E,R,D,H)|Exams],Valid,Conflicts):-
	check(Exams,V,C),
	duration(E,Duration),
	F is H + Duration,
	NewEntry = exam(E,R,D,H,F),
	(conflict(NewEntry,V) ->
		(Valid = V, Conflicts = [E|C]);
		(Valid = [NewEntry|V], Conflicts = C)).

schedule([],[]).
schedule([exam(E,R,D,H,F)|Exams],[event(E,R,D,H)|Events]):-
	schedule(Exams,Events),
	duration(E,Duration),
	F is H + Duration.

fix(X-X,[],_). %we only care for one fix
fix([NewEntry|Events]-X,[E|Exams],Valid):-
	NewEntry = exam(E,R,D,H,F),
	random_result(room_suitable(E,R)),
	random_result(room_available(E,R,D,H,F)),
	not(conflict(NewEntry,Valid)),
	fix(Events-X,Exams,[NewEntry|Valid]).

repair(Schedule,FixedSchedule):-
	random_permutation(Schedule,Events),
	check(Events,Valid,Conflicting),
	fix(Full-Valid,Conflicting,Valid),
	schedule(Full,FixedSchedule),
	!. % we only want one reparation

complete(Schedule,Missing,FixedSchedule):-
	schedule(Exams,Schedule),
	fix(NewExams-[],Missing,Exams),
	schedule(NewExams,NewEvents),
	append(NewEvents,Schedule,FixedSchedule),
	!. % we only want one completion



	





%%% ASSERTING SCHEDULES %%%

assert_schedule([]).
assert_schedule([event(E,_,D,H)|Evs]):-
	duration(E,Duration),
	F is H + Duration,
	asserta(c_event(E,D,H,F)),
	assert_schedule(Evs).
assert_schedule([exam(E,_,D,H,F)|Exams]):-
	asserta(c_event(E,D,H,F)),
	assert_schedule(Exams).

retract_current_schedule:-
	retractall(c_event(_,_,_,_)).

violates_sc(schedule(EventList),SCL):-
	setup_assertions,
	assert_schedule(EventList),
	findall(SC,violates_sc(SC),SCL),
	retract_current_schedule.

exams_violate_sc(ExamList,SCL):-
	assert_schedule(ExamList),
	findall(SC,violates_sc(SC),SCL),
	retract_current_schedule.

violates_sc(sc_lunch_break(PID,Exam,Penalty)):-
	exam_between(Exam,_,12,13),
	exam_person(Exam,PID),
	sc_lunch_break(PID,Penalty).

violates_sc(sc_no_exam_in_period(LID,Exam,Day,From,Till,Penalty)):-
	sc_no_exam_in_period(LID,Day,From,Till,Penalty),
	exam_lecturer(Exam,LID),
	exam_between(Exam,Day,From,Till).

violates_sc(sc_not_in_period(PID,EID,Day,From,Till,Penalty)):-
	sc_not_in_period(PID,EID,Day,From,Till,Penalty),
	exam_between(EID,Day,From,Till).

violates_sc(sc_same_day(PID,EID1,EID2,Penalty)):-
	c_event(EID1,Day,_,_),
	c_event(EID2,Day,_,_),
	EID1 @< EID2, 
	shared_exam_person(EID1,EID2,PID),
	sc_same_day(PID,Penalty).

violates_sc(sc_b2b(PID,EID1,EID2,Penalty)):-
	c_event(EID1,Day,_,Hour),
	c_event(EID2,Day,Hour,_),
	shared_exam_person(EID1,EID2,PID),
	sc_b2b(PID,Penalty).

violates_sc(sc_study_time(SID,DTL,TotalPenalty)):-
	first_day(FD),
	bagof((D,T),
		  E^H^F^(takes_exam(SID,E),
		         c_event(E,D,H,F),
		         sc_study_time(E,T)),
		  UnsortedExams),
	sort(1,@=<,UnsortedExams,Exams),
	student_penalty(Exams,FD,0,DTL),
	DTL > 0,
	sc_study_penalty(SID,Penalty),
	TotalPenalty is DTL * Penalty.

violates_sc(sc_correction_time(LID,DTL,TotalPenalty)):-
	last_day(LD),
	bagof((D,T),
		   E^H^F^(exam_lecturer(E,LID),
		          c_event(E,D,H,F),
		   	      sc_correction_time(E,T)),
		   UnsortedExams),
	sort(1,@>=,UnsortedExams,Exams),
	lecturer_penalty(Exams,LD,0,DTL),
	DTL > 0,
	sc_correction_penalty(LID,Penalty),
	TotalPenalty is DTL * Penalty.

exam_person(Exam,PID):-
	exam_lecturer(Exam,PID).
exam_person(Exam,PID):-
	takes_exam(PID,Exam).

exam_between(EID,Day,From,Till):-
	c_event(EID,Day,Start,Stop),
	overlap(Start,Stop,From,Till).

shortage(Available,Needed,Shortage):-
	ShortageGap is Needed - Available,
	max(0,ShortageGap,Shortage).

shared_exam_person(EID1,EID2,PID):-
	exam_lecturer(EID1,PID),
	exam_lecturer(EID2,PID).

shared_exam_person(EID1,EID2,PID):-
	share_students(EID1,EID2,S),
	member(PID,S).

student_penalty([],_,DTL,DTL).
student_penalty([(Day,TimeNeeded)|Es],LastDay,DTL,Total):-
	AvailableTime is Day - LastDay,
	shortage(AvailableTime,TimeNeeded,Shortage),
	NewDTL is DTL + Shortage,	
	NextDay is LastDay + TimeNeeded,
	min(Day,NextDay,ActualNextDay),
	student_penalty(Es,ActualNextDay,NewDTL,Total).

lecturer_penalty([],_,DTL,DTL).
lecturer_penalty([(Day,TimeNeeded)|Es],LastDay,DTL,Total):-
	AvailableTime is LastDay - Day,
	shortage(AvailableTime,TimeNeeded,Shortage),
	NewDTL is DTL + Shortage,
	NextDay is LastDay - TimeNeeded,
	max(Day,NextDay,ActualNextDay),
	lecturer_penalty(Es,ActualNextDay,NewDTL,Total).

%%%

penalty(SC,Penalty):-
	functor(SC,_,N),
	arg(N,SC,Penalty).

person(SC,PID):- 	
	arg(1,SC,PID).

sc_exam(sc_lunch_break(_,E,_),E).
sc_exam(sc_no_exam_in_period(_,E,_,_,_,_),E).
sc_exam(sc_not_in_period(_,E,_,_,_,_),E).
sc_exam(sc_same_day(_,E,_,_),E).
sc_exam(sc_same_day(_,_,E,_),E).

actual_penalty(SC,ActualPenalty):-
	person(SC,PID),
	student(PID,_),
	!, %green cut
	penalty(SC,Penalty),
	student_count(StudentCount),
	ActualPenalty is Penalty/StudentCount.
actual_penalty(SC,ActualPenalty):-
	person(SC,PID),
	lecturer(PID,_),
	penalty(SC,Penalty),
	lecturer_count(LecturerCount),
	ActualPenalty is Penalty/LecturerCount.

compare_sc(Delta,SC1,SC2):-
	penalty(SC1, P1),
	penalty(SC2, P2),
	(P1 < P2 -> Delta = > ;
	 P1 = P2 -> Delta = = ;
	 otherwise -> Delta = <).

violates_sorted_sc(Schedule,SC):-
	violates_sc(Schedule,UnsortedSC),
	predsort(compare_sc,UnsortedSC,SC).


%%% 

exams_cost(Schedule,Cost):-
	exams_violate_sc(Schedule,Constraints),
	constraint_costs(Constraints,Cost).

cost(Schedule,Cost):-
	violates_sc(Schedule,Constraints),
	constraint_costs(Constraints,Cost).

constraint_costs(Constraints,Cost):-
	student_count(AmountOfStudents),
	lecturer_count(AmountOfLecturers),
	constraint_costs(Constraints,0,0,SC,LC),
	AvgStudentCost is SC/AmountOfStudents,
	AvgLecturerCost is LC/AmountOfLecturers,
	Cost is (AvgStudentCost + AvgLecturerCost)/2.
	
constraint_costs([],SC,LC,SC,LC).
constraint_costs([C|CS],CurrentSC,CurrentLC,SC,LC):-
	person(C,PID),
	student(PID,_),
	!, %green cut <- notice that students greatly outnumber lecturers!
	penalty(C,Penalty),
	NewSC is CurrentSC + Penalty,
	constraint_costs(CS,NewSC,CurrentLC,SC,LC).
constraint_costs([C|CS],CurrentSC,CurrentLC,SC,LC):-
	person(C,PID),
	lecturer(PID,_),
	penalty(C,Penalty),
	NewLC is CurrentLC + Penalty,
	constraint_costs(CS,CurrentSC,NewLC,SC,LC).


%%%

is_optimal(X):-
	findall((S,C),(is_valid(S),cost(S,C)),Schedules),
	optimal_schedules(Schedules,Optimals),
	member(X,Optimals).

optimal_schedules([(Schedule,Cost)|Schedules],Optimals):-
	optimal_schedules(Schedules,Cost,[Schedule],Optimals).
	
optimal_schedules([],_,Optimals,Optimals).
optimal_schedules([(S,C)|Schedules],C,Current,Optimals):-
	!, %green cut
	optimal_schedules(Schedules,C,[S|Current],Optimals).
optimal_schedules([(_,C)|Schedules],Cost,Current,Optimals):-
	Cost < C,
	!, %green cut
	optimal_schedules(Schedules,Cost,Current,Optimals).
optimal_schedules([(S,C)|Schedules],Cost,_,Optimals):-
	C < Cost,
	!, %green cut
	optimal_schedules(Schedules,C,[S],Optimals).


%%% SIMULATED ANNEALING %%%

find_h(schedule(Schedule),T):-
	get_time(StartTime),
	MaxTime is StartTime + T,
	is_valid(schedule(S)),
	cost(schedule(S),Cost),
	!,
	beam_search([(S,Cost)],20,MaxTime,Schedule).

% when run out of time, we return the 'fittest'
beam_search([(Schedule,_)|_],_,MaxTime,Schedule):-
	get_time(CurrentTime),
	CurrentTime >= MaxTime,
	!. % time is up; stop searching...
beam_search(Beam,N,MaxTime,Schedule):-
	Beam = [(_,LowC)|_], write(LowC), nl,
	findall((NewSchedule,NewCost),
			(member((S,_),Beam),
			 mutation(S,NewSchedule),
			 cost(schedule(NewSchedule),NewCost)),
			Candidates,Beam),
	sort(2,@=<,Candidates,SortedCandidates),
	take(SortedCandidates,N,NewBeam-[],_),
	beam_search(NewBeam,N,MaxTime,Schedule).



%%% GENETIC APPROACH %%%


crossover(S1,S2,Cross):-
	length(S1,ExamCount), %TODO: assert statically
	sort(1,@=<,S1,SortedS1),
	sort(1,@=<,S2,SortedS2),
	random_between(0,ExamCount,N),
	take(SortedS1,N,Child1-RestS2,RestS1),
	take(SortedS2,N,Child2-RestS1,RestS2),
	(C = Child1 ; C = Child2),
	repair(C,Cross).

mutation(EventList,Mutation):-
	random_select(event(E,_,_,_),EventList,Remaining),
	complete(Remaining,[E],Mutation).

 
%%% FITNESS %%%

fitness(Schedule,Fitness):-
	cost(schedule(Schedule),Cost),
	Fitness is 1/(1+Cost).

total_fitness([],0).
total_fitness([(_,F)|Rest],TF):-
	total_fitness(Rest,FRest),
	TF is FRest + F.

fittest([Fittest],Fittest).
fittest([(S1,F1)|R],(SF,FF)):-
	fittest(R,(SF,FF)),
	F1 < FF,
	!. % necessary red cut
fittest([X|_],X).

%%% INITIAL POPULATION %%%

population_randomness(50).
population_size(50).

% X is a random schedule after C mutations
random_schedule(X,C):-	
	is_valid(schedule(S)),
	random_schedule(S,C,X).

random_schedule(X,0,X).
random_schedule(X,C,S):-
	C > 0,
	C1 is C - 1,
	mutation(X,Y),
	!, % don't backtrack on one mutation
	random_schedule(Y,C1,S).

initial_population(0,_,[]).
initial_population(PopSize,Randomness,[(X,F)|Pop1]):-
	PopSize > 0,
	PopSize1 is PopSize - 1,
	initial_population(PopSize1,Randomness,Pop1),
	random_schedule(X,Randomness),
	!, % only one random schedule.
	fitness(X,F).

%%% MATING POOL %%%

% roulette wheel algorithm

selection(Population,TargetSize,Selected):-
	total_fitness(Population,TotalFitness),
	selection((Population,Population),TotalFitness,TargetSize,Selected).

selection(_,_,0,[]):- !.
selection(([],Pop),TF,TS,Selected):-
	selection((Pop,Pop),TF,TS,Selected).
selection(([(S,F)|R],P),TF,TS,[S|Selected]):-
	Odds is F/TF,
	maybe(Odds),
	!, % S was selected
	TS1 is TS - 1,
	selection((R,P),TF,TS1,Selected).
selection(([_|R],P),TF,TS,Selected):-
	selection((R,P),TF,TS,Selected).

offspring([S1,S2|_],S):-
	crossover(S1,S2,S).
offspring([_,_|R],Offspring):-
	offspring(R,Offspring).

%%% GENETIC ALGORITHM %%%


find_heuristically(schedule(Schedule),T):-
	% register maximum delivery time
	get_time(StartTime),
	MaxTime is StartTime + T,
	% initialize initial population
	population_randomness(Randomness),
	population_size(PopSize),
	initial_population(PopSize,Randomness,Pop),
	% start evolution of first generation
	evolution(Pop,PopSize,MaxTime,Schedule).

% when run out of time, we return the 'fittest'
evolution(Pop,_,MaxTime,Fittest):-
	get_time(CurrentTime),
	CurrentTime >= MaxTime,
	!,
	fittest(Pop,(Fittest,_)).

evolution(Pop,PopSize,MaxTime,Schedule):-
	length(Pop,N), 
	write('current generation size: '), write(N), nl,
	%findall((Mutation,Fitness),
	%		(member((S,_),Pop),
	%		 mutation(S,Mutation),
	%		 fitness(Mutation,Fitness)),
	%		Full,Pop),
	selection(Pop,PopSize,MatingPool),
	findall((Schedule,Fitness),
			(offspring(MatingPool,Child),
			 mutation(Child,Schedule),
			 fitness(Schedule,Fitness)),
			NextGeneration_),
	%sort(2,@>,Pop2,SortedSchedules),
	%take(SortedSchedules,PopSize,NextGeneration-[],_),
	random_permutation(NextGeneration_,NextGeneration),
	evolution(NextGeneration,PopSize,MaxTime,Schedule).











%%% NOTES %%%
% could be faster with
% 	- specialized data structure
%	- keeping things sorted
% however...
% 	- more procedural vs declarative
%	- faster to generate, but not always to check! 

%TODO: 	
%	optimizations: is_valid/1
%		- sorted list
%		- free timeslots
%	optimization: cost/1
%		- group similar exam schedules
%	error-handling: float in `between`



