:- dynamic share_students/3.
:- dynamic required_capacity/2.
:- dynamic student_count/1.
:- dynamic lecturer_count/1.
:- dynamic setup_completed/0.
:- dynamic c_examHours/3.
:- dynamic c_examRoom/2.
:- dynamic c_examDay/2.
:- discontiguous
	 sc_lunch_break/2,
	 sc_not_in_period/6,
	 sc_same_day/2,
	 sc_b2b/2.

lecturer(l1,'Mr John').
lecturer(l2,'Mr Francis').
lecturer(l3,'Mr Josef').
lecturer(l4,'Ms Ann').

student(s1,'Anna').
student(s2,'Max').
student(s3,'Bill').
student(s4,'Carla').

course(c1,'Math').
course(c2,'Science & Technology').
course(c3,'Philosophy').
course(c4,'Religion').
course(c5,'English').

exam(e1,'Math').
exam(e2,'Science & Technology').
exam(e3,'Philosophy').
exam(e4,'Religion').
exam(e5,'English').

room(r1,'Small room').
room(r2,'Large room').

has_exam(c1,e1).
has_exam(c2,e2).
has_exam(c3,e3).
has_exam(c4,e4).
has_exam(c5,e5).

duration(Exam,2) :- exam(Exam,_). %every exam takes 2 hours

follows(Student,c1) :- student(Student,_). %every student follows Math
follows(Student,c2) :- student(Student,_). %every student follows Science & Technology
follows(s2,c3). %Max follows philosophy
follows(s3,c3). %Bill follows philosophy
follows(s1,c4). %Anna follows religion
follows(s4,c4). %Carla follows religion
follows(Student,c5) :- student(Student,_). %every student follows Languages

teaches(l1,c1).
teaches(l1,c2).
teaches(l2,c3).
teaches(l3,c4).
teaches(l4,c5).

capacity(r1,2).
capacity(r2,4).

%first and last day of exam period
first_day(1).
last_day(5).

%Rooms are available
availability(Room,1,10,12) :- room(Room,_). %Day 1, all rooms are available from 10 to 12
availability(Room,2,10,12) :- room(Room,_). %Day 2, all rooms are available from 10 to 12
availability(Room,3,10,15) :- room(Room,_). %Day 3, all rooms are available from 10 to 15
availability(Room,4,10,12) :- room(Room,_). %Day 4, all rooms are available from 10 to 12
availability(Room,5,10,12) :- room(Room,_). %Day 5, all rooms are available from 10 to 12

%soft-constraints
%lecturer
sc_lunch_break(L,1) :- lecturer(L,_). %lecturers prefer a lunchbreak
sc_b2b(L,2) :- lecturer(L,_). %lecturers prefer not to have exams back 2 back
sc_no_exam_in_period(l3,3,0,24,5). %Josef prefers no exams at day 3
sc_no_exam_in_period(l4,Day,0,12,1) :- first_day(FirstDay),last_day(LastDay),between(FirstDay,LastDay,Day). %Ann prefers no exams before noon
sc_no_exam_in_period(l1,Day,14,24,5) :- first_day(FirstDay),last_day(LastDay),between(FirstDay,LastDay,Day). %John prefers no exams after 14h
sc_not_in_period(l1,e2,1,0,24,3). %Science & technology preferably not day 1
sc_correction_time(e1,2). 
sc_correction_time(e2,1).
sc_correction_time(e3,1).
sc_correction_time(e4,1).
sc_correction_time(e5,2).
sc_correction_penalty(L,3) :- lecturer(L,_). %guarantee enough correction time

%student
sc_lunch_break(S,1) :- student(S,_). %students prefer a lunchbreak
sc_same_day(S,2) :- student(S,_). %students prefer not to have multiple exams on the same day
sc_b2b(S,5) :- student(S,_). %students prefer not to have exams back 2 back
sc_study_time(e1,2). 
sc_study_time(e2,1).
sc_study_time(e3,1).
sc_study_time(e4,1).
sc_study_time(e5,1).
sc_study_penalty(S,3) :- student(S,_). %guarantee enough study time


%%% PROJECT STARTS HERE %%%


%%% UTILS

remove_one([E|R],E,R).
remove_one([H|T],E,[H|Z]):-
	H \= E,
	remove_one(T,E,Z).

overlap(H1,F1,H2,_):- 
	H1 =< H2, 
	F1 > H2.
overlap(H1,_,H2,F2):- 
	H1 > H2,
	H1 < F2.

member(E,[E|_]).
member(E,[_|T]):-
	member(E,T).

max(M,N,M):- M>=N,!.
max(M,N,N):- M<N.

min(M,N,M):- M=<N,!.
min(M,N,N):- M>N.

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

%%% RUN-TIME ASSERTIONS

assert_shared_exam_students:-
	bagof(S,takes_exams(S,E1,E2),L),
	asserta(share_students(E1,E2,L)).

assert_exam_capacity:-
	bagof(S,takes_exam(S,E),L),
	length(L,N),
	asserta(required_capacity(E,N)).

assert_student_count:-
	findall(_,student(_,_),L),
	length(L,N),
	asserta(student_count(N)).

assert_lecturer_count:-
	findall(_,lecturer(_,_),L),
	length(L,N),
	asserta(lecturer_count(N)).

setup_assertions:-
	setup_completed,
	!. %green cut	
setup_assertions:-
	not(setup_completed),
	assert_student_count,
	assert_lecturer_count,
	findall(_,assert_exam_capacity,_),
	findall(_,assert_shared_exam_students,_),
	asserta(setup_completed).

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
	findall(E,exam(E,_),Exams),
	is_valid(EventList,Exams,[]).

is_valid([],[],_).
is_valid([event(Exam,Room,Day,Hour)|Events],Exams,Reservations):-
	remove_one(Exams,Exam,Remaining),
	!, %red cut <- consider only one exam each time
	room_suitable(Exam,Room),
	room_available(Exam,Room,Day,Hour,End),
	NewEntry = exam(Exam,Room,Day,Hour,End),
	not(conflict(NewEntry,Reservations)),
	is_valid(Events,Remaining,[NewEntry|Reservations]).

%%%%%

assert_schedule([]).
assert_schedule([event(E,R,D,H)|Evs]):-
	duration(E,Duration),
	F is H + Duration,
	asserta(c_examDay(E,D)),
	asserta(c_examRoom(E,R)),
	asserta(c_examHours(E,H,F)),
	assert_schedule(Evs).

retract_current_schedule:-
	retractall(c_examDay(_,_)),
	retractall(c_examRoom(_,_)),
	retractall(c_examHours(_,_,_)).

violates_sc(schedule(EventList),SCL):-
	setup_assertions,
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
	c_examDay(EID1,Day),
	c_examDay(EID2,Day),
	EID1 @< EID2, 
	shared_exam_person(EID1,EID2,PID),
	sc_same_day(PID,Penalty).

violates_sc(sc_b2b(PID,EID1,EID2,Penalty)):-
	c_examDay(EID1,Day),
	c_examDay(EID2,Day),
	c_examHours(EID1,_,Hour),
	c_examHours(EID2,Hour,_),
	shared_exam_person(EID1,EID2,PID),
	sc_b2b(PID,Penalty).

violates_sc(sc_study_time(SID,DTL,TotalPenalty)):-
	first_day(FD),
	bagof((D,T),
		  E^(takes_exam(SID,E),
		     c_examDay(E,D),
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
		   E^(exam_lecturer(E,LID),
		   	  c_examDay(E,D),
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
	c_examDay(EID,Day),
	c_examHours(EID,Start,Stop),
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
optimal_schedules([(S,C)|Schedules],Cost,Current,Optimals):-
	Cost < C,
	!, %green cut
	optimal_schedules(Schedules,Cost,Current,Optimals).
optimal_schedules([(S,C)|Schedules],Cost,Current,Optimals):-
	C < Cost,
	!, %green cut
	optimal_schedules(Schedules,C,[S],Optimals).




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



