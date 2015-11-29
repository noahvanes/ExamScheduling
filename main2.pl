:- dynamic share_students/3.

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



%%% PROJECT STARTS HERE %%%


remove_one([E|R],E,R).
remove_one([H|T],E,[H|Z]):-
	H \= E,
	remove_one(T,E,Z).

takes_exam(Student,Exam):-
	has_exam(Course,Exam),
	follows(Student,Course).

required_capacity(Exam,RequiredCapacity):-
	findall(S,takes_exam(S,Exam),Students),
	length(Students,RequiredCapacity).

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

%conflicts can only occur on the same Day
conflict(NewEntry,[Exam|_]):-
	overlap(NewEntry,Exam), 	%do the scheduled hours of both exams overlap?
	problem(NewEntry,Exam). 	%is it a problem to hold both exams concurrently?
conflict(NewExam,[_|Exams]):-
	conflict(NewExam,Exams).

overlap(exam(_,_,D,H1,F1),exam(_,_,D,H2,_)):- 
	H1 =< H2, 
	F1 > H2.
overlap(exam(_,_,D,H1,_),exam(_,_,D,H2,F2)):- 
	H1 >= H2,
	H1 < F2.

problem(exam(_,R,_,_,_),exam(_,R,_,_,_)).		%two overlapping exams in the same room
problem(exam(E1,_,_,_,_),exam(E2,_,_,_,_)):-	%two overlapping exams with overlapping participants
	mutual_exclusive(E1,E2).

exam_lecturer(Exam,Lecturer):-
	has_exam(Course,Exam),
	lecturer(Course,Lecturer).

mutual_exclusive(E1,E2):-
	exam_lecturer(E1,L),
	exam_lecturer(E2,L).
mutual_exclusive(E1,E2):-
	share_students(E1,E2,_).

takes_exams(S,E1,E2):-
	takes_exam(S,E1),
	takes_exam(S,E2),
	E1 \= E2.

assert_exam_students:-
	bagof(S,takes_exams(S,E1,E2),L),
	asserta(share_students(E1,E2,L)).

setup_assertions:-
	%TODO: some assertion check
	findall(_,assert_exam_students,_).

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

mysched(schedule([
	event(e3, r1, 3, 10), 
	event(e5, r2, 4, 10),
	event(e1, r2, 1, 10), 
	event(e2, r2, 2, 10), 
	event(e4, r1, 3, 12)
])).

violates_sc(schedule(EventList),SC):-
	schedule(EventList,Schedule),
	findall([1,2,3|X]



schedule([],[]).
schedule([E|Es],Schedule):-
	schedule(Es,ScheduleRest),
	add_event(E,ScheduleRest,Schedule).

add_event(event(E,_,D,H),[],[day(D,L)]):-
	add_exam(exam(E,H),[],L).
add_event(event(E,_,D,H),[day(D,L)|Ds],[day(D,NL)|Ds]):-
	add_exam(exam(E,H),L,NL).
add_event(event(E,_,D1,H),[day(D2,L2)|Ds],[day(D1,L1),day(D2,L2)|Ds]):-
	D1 < D2,
	add_exam(exam(E,H),[],L1).
add_event(event(E,_,D1,H),[day(D2,L2)|Ds],[day(D2,L2)|Dz]):-
	D1 > D2,
	add_event(event(E,_,D1,H),Ds,Dz).

add_exam(E,R,[E|R]).




%%% NOTES %%%
% could be faster with
% 	- specialized data structure
%	- keeping things sorted
% however...
% 	- more procedural vs declarative
%	- faster to generate, but not always to check! 



