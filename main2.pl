
:- dynamic c_event/4.

:- use_module(dataset_large).
:- use_module(kb_assertions).
:- use_module(utils).

%:- use_module(library(sort)).


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


%%% EXTRA: completing incomplete/partial schedules %%%

%% scheduled_exams(+Events,-Exams,-Remaining) <- Exams are the corresponding
%%												 exam entries for the partial
%%												 schedule of Events; Remaining
%%												 is the list of exams that still
%%												 need to be scheduled after this.
scheduled_exams([],[],E):- 
	exams(E).
scheduled_exams([event(E,R,D,H)|T],[exam(E,R,D,H,F)|Z],Remaining):-
	scheduled_exams(T,Z,OldRemaining),
	remove_one(OldRemaining,E,Remaining),
	duration(E,Duration),
	F is H + Duration.

%% complete(?Events,+PartialSchedule) <- Given a valid PartialSchedule
%%										 of exams, Events complete this
%%										 schedule so to include all exams
complete(Events,PartialSchedule):-
	scheduled_exams(PartialSchedule,Entries,Remaining),
	is_valid(Events,Remaining,Entries).


%%% ASSERTING SCHEDULES %%%

assert_schedule([]).
assert_schedule([event(E,_,D,H)|Evs]):-
	duration(E,Duration),
	F is H + Duration,
	asserta(c_event(E,D,H,F)),
	assert_schedule(Evs).

retract_current_schedule:-
	retractall(c_event(_,_,_,_)).

violates_sc(schedule(EventList),SCL):-
	setup_assertions,
	violates_sc(EventList,SCL).

violates_sc(EventList,SCL):-
	assert_schedule(EventList),
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








%% a random split

random_split([],_,[],[]).
random_split([X|T],Rate,S1,[X|S2]):-
	maybe(Rate),
	!,
	random_split(T,Rate,S1,S2).
random_split([X|T],Rate,[X|S1],S2):-
	random_split(T,Rate,S1,S2).

%% a random day


mutation_rate(0.03).

mutation(EventList,MutatedEventList):-
	mutation_rate(MR),
	random_split(EventList,MR,Keep,Drop),
	find((random_events(Drop,MutatedEvents),
	      complete(MutatedEvents,Keep))),
	!, %search for only one mutation
	append(MutatedEvents,Keep,MutatedEventList).


%% THE REAL ATTEMPT!

random_day(D):-
	first_day(FD),
	last_day(LD),
	random_between(FD,LD,D).

random_room(E,R):-
	findall(RID,room_suitable(E,RID),Rooms),
	random_member(R,Rooms).

random_hour(E,R,D,H):-
	findall((S,F),availability(R,D,S,F),Slots),
	random_member((Start,Stop),Slots),
	duration(E,Duration),
	LatestStart is Stop - Duration,
	random_between(Start,LatestStart,H).

random_events([],[]).
random_events([event(E,_,_,_)|T],[event(E,R,D,H)|Z]):-
	random_day(D),
	random_room(E,R),
	random_hour(E,R,D,H),
	random_events(T,Z).

%% find
find(X):- 
	call(X).
find(X):- 
	find(X).

%%
selection(Schedules,MaxSize,Survivors):-
	sort(2,@=<,Schedules,SortedSchedules),
	take(SortedSchedules,MaxSize,Survivors).


find_heuristically(Schedule,Cost,T):-
	get_time(StartTime),
	MaxTime is StartTime + T,
	is_valid(schedule(PopulationOrigin)),
	cost(schedule(PopulationOrigin),C),
	!,
	evolution([(PopulationOrigin,C)],100,MaxTime,Schedule,Cost).

evolution([(Schedule,Cost)|_],_,MaxTime,Schedule,Cost):-
	get_time(CurrentTime),
	CurrentTime >= MaxTime,
	!.

evolution(Population,MaxSize,MaxTime,Schedule,Cost):-
	findall((Schedule,Cost),
			(member((Original,_),Population),
			 mutation(Original,Schedule),
			 cost(schedule(Schedule),Cost)),
			OversizedNextGeneration,
			Population),
	selection(OversizedNextGeneration,MaxSize,NextGeneration),
	evolution(NextGeneration,MaxSize,MaxTime,Schedule,Cost).


	











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



