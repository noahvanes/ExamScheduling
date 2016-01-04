:- dynamic c_event/4.

:- use_module(dataset_short).
:- use_module(kb_assertions).
:- use_module(utils).

%%% LAST TRY

goal(([],E),E).

exams([],[]).
exams([(E,_)|T],[E|Z]):-
	exams(T,Z).

initial_state((E,[])):-
	findall((E,N),
			(bagof(S,takes_exam(S,E),L),
			 length(L,N)),
			Exams),
	sort(2,@>=,Exams,SortedExams),
	exams(SortedExams,E).

%alternative(Exams,AlternativeExams):-	
%	random_select(exam(E1,_,_,_,_),Exams,Remaining),
%	successor(([E1],Remaining),([],AlternativeExams)).

state_cost((_,E),C):-
	exams_cost(E,C).

successor(([Exam|R],Exams),(R,[NewEntry|Exams])):-
	room_suitable(Exam,Room),
	room_available(Exam,Room,Day,Hour,End),
	NewEntry = exam(Exam,Room,Day,Hour,End),
	not(conflict(NewEntry,Exams)).

random_successor(([Exam|R],Exams),(R,[NewEntry|Exams])):-
	find_random(room_suitable(Exam,Room)),
	find_random(room_available(Exam,Room,Day,Hour,End)),
	NewEntry = exam(Exam,Room,Day,Hour,End),
	not(conflict(NewEntry,Exams)).

find(schedule(S),T):-
	% register deadlines
	Period is T/2,
	get_time(StartTime),
	Deadline1 is StartTime + Period,
	Deadline2 is Deadline1 + Period,
	% we need a dummy schedule to compare to
	is_valid(schedule(InitEvents)),
	!, % only one schedule needed
	schedule(InitExams,InitEvents),
	exams_cost(InitExams,InitCost),
	% phase 1: (greedy) branch-and-bound DFS
	initial_state(I),
	search([(I,0)],Deadline1,InitExams,InitCost,Exams,Cost),
	% phase 2: (optimization) local beam search
	min(Period,300,BeamWidth),
	beam_search([(Exams,Cost)],BeamWidth,Deadline2,X),
	% output in desired format
	schedule(X,S).

search(_,Deadline,X,C,X,C):-
	get_time(CurrentTime),
	CurrentTime >= Deadline,
	!. %stop the search ...
search([(_,SC)|R],Deadline,BestS,BestC,X,C):-
	SC >= BestC,
	!,
	search(R,Deadline,BestS,BestC,X,C).
search([(State,SC)|R],Deadline,_,BestC,X,C):-
	goal(State,SE),
	SC < BestC,
	!,
	search(R,Deadline,SE,SC,X,C).
search([(State,_)|R],Deadline,BestS,BestC,X,C):-
	findall((NewState,NewCost),
			(successor(State,NewState),
		 	 state_cost(NewState,NewCost)),
			Children),
	sort(2,@=<,Children,SortedChildren),
	append(SortedChildren,R,NewAgenda),
	!,
	search(NewAgenda,Deadline,BestS,BestC,X,C).
search([],_,X,C,X,C).

beam_search([(X,_)|_],_,Deadline,X):-
	get_time(CurrentTime),
	CurrentTime >= Deadline,
	!. % stop searching...
beam_search(CurrentBeam,N,Deadline,X):-
	findall((NewExams,NewCost),
			(member((Exams,_),CurrentBeam),
			 mutation(Exams,NewExams),
			 !, %only one mutation per schedule
			 exams_cost(NewExams,NewCost)),
			CandidateStates,
			CurrentBeam),
	sort(2,@<,CandidateStates,SortedStates),
	take(SortedStates,N,NewBeam,_),
	!,
	beam_search(NewBeam,N,Deadline,X).

%%% EXAM PROPERTIES





%%% IS_VALID

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

find_random(X):-
	findall(X,X,Results),
	random_permutation(Results,RandomResults),
	member(X,RandomResults).


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
	students_exams(Students,Exams),
	findall((D,T),
		  	(member(E,Exams),
		     c_event(E,D,_,_),
		     sc_study_time(E,T)),
		  UnsortedExams),
	sort(1,@=<,UnsortedExams,SortedExams),
	student_penalty(SortedExams,FD,0,DTL),
	DTL > 0,
	member(SID,Students),
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


mutation(Exams,Mutation):-
	random_select(exam(E1,_,_,_,_),Exams,R1),
	random_select(exam(E2,_,_,_,_),R1,R2),
	random_successor(([E1,E2],R2),IntermediateState),
	random_successor(IntermediateState,([],Mutation)).

%%% printing %%%

day_schedule(EventList,Day,DaySchedule):-
	findall((D,S),
			bagof((E,R,H),member(event(E,R,D,H),EventList),S),
			UnsortedSchedules),
	sort(1,@=<,UnsortedSchedules,Schedules),
	member((Day,DaySchedule),Schedules).

room_schedule(DaySchedule,Room,RoomSchedule):-
	bagof((E,H),
		  member((E,Room,H),DaySchedule),
		  RoomSchedule).

exam_entry(RoomSchedule,Hour,Exam):-
	sort(2,@<,RoomSchedule,SortedSlots),
	member((Exam,Hour),SortedSlots).
	
pretty_print_day(Day):-
	nl,
	write('*** DAY '),
	write(Day),
	write(' ***'),
	nl.

pretty_print_room(Room):-
	nl,
	room(Room,RoomName),
	write(RoomName),
	nl.

pretty_print_exam(Hour,Exam):-
	write(Hour),
	write(':00 '),
	exam(Exam,ExamName),
	exam_lecturer(Exam,LID),
	lecturer(LID,LecturerName),
	write(ExamName),
	write(' ('),
	write(LecturerName),
	write(')'),
	nl.

student_filter(_,[],[]).
student_filter(SID,[event(E,_,_,_)|T],Z):-
	not(takes_exam(SID,E)),
	!,
	student_filter(SID,T,Z).
student_filter(SID,[E|T],[E|Z]):-
	student_filter(SID,T,Z).

pretty_print(schedule(Schedule),SID):-
	student_filter(SID,Schedule,StudentSchedule),
	pretty_print(schedule(StudentSchedule)).

pretty_print(schedule(Schedule)):-
	findall(_,pretty(Schedule),_),
	nl. % put newline after schedule

pretty(EventList):-
	day_schedule(EventList,Day,DaySchedule),
	pretty_print_day(Day),
	room_schedule(DaySchedule,Room,RoomSchedule),
	pretty_print_room(Room),
	exam_entry(RoomSchedule,Hour,Exam),
	pretty_print_exam(Hour,Exam).






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



