%%% MODULE VIOLATES_SC %%%

/**
	The predicate exported in this module is able to list all soft constraints that are violated by a schedule.
	They are returned as a list with their penalty costs in the same format as specified in the assignment.
*/

:- module(violates_sc,
	[violates_sc/2,
	 exams_violate_sc/2,
	 penalty/2,
	 person/2]).

:- dynamic c_event/4.


%%% IMPORTS %%%

:- use_module(setup, [setup_assertions/0,share_student/3,students_exams/2]).
:- use_module(utils, [max/3,min/3,member/2,overlap/4]).
:- use_module(exams, [exam_lecturer/2,takes_exam/2]).


%%% VIOLATES_SC %%% 

%% violates_sc(+Schedule,-SoftConstraints)
%%     SoftConstraints is a list of soft constraints violated by Schedule
%%	   Schedule should be given in the format specified in the assignment
violates_sc(schedule(EventList),SCL):-
	setup_assertions,
	assert_schedule(EventList),
	findall(SC,violates_sc(SC),SCL),
	retract_current_schedule.

%% exams_violate_sc(+ExamList,-SoftConstraints)
%%     Identical to violates_sc/2, but for internal usage in other modules
%%     Assumes that memoized data is already setup, and uses the internal schedule format
exams_violate_sc(ExamList,SCL):-
	assert_schedule(ExamList),
	findall(SC,violates_sc(SC),SCL),
	retract_current_schedule.


%%% VIOLATION PERSON/PENALTY %%%

%% person(+SoftConstraint,-Person)
%%     Person is suffering the penalty of SoftConstraint
%%	   The affected person is always the first element of a soft constraint
person(SC,PID):- 	
	arg(1,SC,PID).

%% penalty(+SoftConstraint,-Penalty)
%%     Penalty is the cost of violating SoftConstraint
%%     This penalty is always the last element of a soft constraint
penalty(SC,Penalty):-
	functor(SC,_,N),
	arg(N,SC,Penalty).


%%% CHECKING VIOLATIONS %%%

%% violates_sc(?ViolatedSC)
%%     ViolatedSC is a violated soft constraint of the current schedule.
%%	   Can be used to either check for a specific violation or generate them.
%%     The formats are the same as those specified in the assignment.

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


%%% ASSERTING SCHEDULES %%%

%% assert_schedule(+Schedule)
%%     Asserts a given schedule in Prologs KB
%%     Makes it easier for violates_sc/1 to find violations in the current schedule
assert_schedule([]).
assert_schedule([event(E,_,D,H)|Evs]):-
	duration(E,Duration),
	F is H + Duration,
	asserta(c_event(E,D,H,F)),
	assert_schedule(Evs).
assert_schedule([exam(E,_,D,H,F)|Exams]):-
	asserta(c_event(E,D,H,F)),
	assert_schedule(Exams).

%% retract_current_schedule
%%     The inverse of assert_schedule
%%     Retracts the currently asserted schedule from Prologs KB
retract_current_schedule:-
	retractall(c_event(_,_,_,_)).


%%% EXAM PREDICATES %%%

%% exam_person(?Exam,?PID)
%%	   PID is involved in Exam when either:
%%	        * PID is the lecturer of Exam
%%			* PID is a student taking Exam
exam_person(Exam,PID):-
	exam_lecturer(Exam,PID).
exam_person(Exam,PID):-
	takes_exam(PID,Exam).

%% shared_exam_person(?EID1,?EID2,?PID)
%%     PID is involved in both exams EID1 and EID2
%%     Not implemented through exam_person/2 for efficiency reasons
shared_exam_person(EID1,EID2,PID):-
	exam_lecturer(EID1,PID),
	exam_lecturer(EID2,PID).
shared_exam_person(EID1,EID2,PID):-
	share_student(EID1,EID2,PID).

%% exam_between(?EID,?Day,+From,+Till)
%%     Exam EID is scheduled on Day, taking place somewhere between From and Till
%%     Mainly used to check if a scheduled exam falls within a certain interval/period
exam_between(EID,Day,From,Till):-
	c_event(EID,Day,Start,Stop),
	overlap(Start,Stop,From,Till).


%%% HELPER PREDICATES FOR STUDY_TIME/CORRECTION_TIME %%%

%% shortage(+Available,+Needed,-Shortage)
%%      Given an amount of Available and Needed time, we have a shortage Shortage 
%%      If there is no real shortage, the actual Shortage is 0.
shortage(Available,Needed,Shortage):-
	ShortageGap is Needed - Available,
	max(0,ShortageGap,Shortage).

%% student_penalty(+Exams,+LastDay,+DTL,-Total)
%%     Exams is a list of exam days and their time needed to study them
%%	   LastDay is the last day UNTIL which 'study time' has already been scheduled
%%     DTL is the current total of insufficient days
%%	   Total is the total count of days too little
%%     (this iteration is used internally for violates_sc/1)
student_penalty([],_,DTL,DTL).
student_penalty([(Day,TimeNeeded)|Es],LastDay,DTL,Total):-
	AvailableTime is Day - LastDay,
	shortage(AvailableTime,TimeNeeded,Shortage),
	NewDTL is DTL + Shortage,	
	NextDay is LastDay + TimeNeeded,
	min(Day,NextDay,ActualNextDay),
	student_penalty(Es,ActualNextDay,NewDTL,Total).

%% lecturer_penalty(+Exams,+LastDay,+DTL,-Total)
%%     Exams is a list of exam days and their time needed to correct them
%%	   LastDay is the last day FROM which 'correction time' has already been scheduled
%%     DTL is the current total of insufficient days
%%	   Total is the total count of days too little
%%     (this iteration is used internally for violates_sc/1)
lecturer_penalty([],_,DTL,DTL).
lecturer_penalty([(Day,TimeNeeded)|Es],LastDay,DTL,Total):-
	AvailableTime is LastDay - Day,
	shortage(AvailableTime,TimeNeeded,Shortage),
	NewDTL is DTL + Shortage,
	NextDay is LastDay - TimeNeeded,
	max(Day,NextDay,ActualNextDay),
	lecturer_penalty(Es,ActualNextDay,NewDTL,Total).