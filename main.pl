%Allow grouping constraints per students/lecturers
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


%%
%%%% PROJECT
%%


% removeOne(?List, ?E, ?NewList) <- remove first occurence of E
%									in List, resulting in NewList
removeOne([E|R],E,R).
removeOne([H|T],E,[H|Z]):-
	H \= E,
	removeOne(T,E,Z).

% full_schedule(-EventList)	<- EventList is a list of events that
%							   provides a schedule for each exam	
full_schedule(EventList):-
	findall(E,exam(E,_),Exams),
	full_schedule(EventList,Exams).

full_schedule([],[]).
full_schedule([event(E1,_,_,_)|Events],Exams):-
	removeOne(Exams,E1,NewExams),
	!, %red cut <- we only want to generate one schedule
	full_schedule(Events, NewExams).

%%%%%%%%%%%%%%

takes_exam(Student,Exam):-
	has_exam(Course,Exam),
	follows(Student,Course).
	
%
% TODO: use setof to avoid duplicates?
%
required_capacity(Exam,RequiredCapacity):-
	findall(S,takes_exam(S,Exam),Students),
	length(Students,RequiredCapacity).

room_suitable(Room,RequiredCapacity):-
	capacity(Room,Capacity),
    RequiredCapacity =< Capacity.

%% TIMETABLE
available_timeslots(T):-
	findall(room(Room,AvailableSlots),
			(bagof(slot(Day,Start,Duration),
			       Stop^(availability(Room,Day,Start,Stop),
				         Duration is Stop - Start),
				         UnsortedSlots),
			 sort(3,@>=,UnsortedSlots,AvailableSlots)),
			T).

free_slot(Slots,event(Exam,R,D,H),NewSlots):-
	duration(Exam,Duration),
	required_capacity(Exam,ReqCapacity),
	free_slot_aux(Slots,exam(ReqCapacity,Duration,R,D,H),NewSlots).
	
free_slot_aux([room(Room,Slots)|Rs],exam(Rc,Dr,Room,Dy,Hr),[room(Room,NewSlots)|Rs]):-
	room_suitable(Room,Rc), %room has required capacity
	slot_from(Slots,exam(Dr,Dy,Hr),NewSlots).
free_slot_aux([Room|T],Exam,[Room|Z]):-
	free_slot_aux(T,Exam,Z).

slot_from([slot(_,_,Duration)|_],exam(Time,_,_),_):-
	Duration < Time, 
	!, % red cut -> optimize, slots are sorted
	fail.
slot_from([slot(Day,Start,Duration)|Rest],exam(Time,Day,Hour),NewSlots):-
	Stop is Start + Duration,
	LatestStart is Stop - Time,
	between(Start,LatestStart,Hour),
	End is Hour + Time,
	split_slot(Day,Hour,End,Start,Stop,Rest,NewSlots).
slot_from([H|T],E,[H|Z]):-
	slot_from(T,E,Z).

%%
%% TODO: with or without red cuts?
%%
split_slot(Day,Start,Stop,Start,Stop,Rest,Rest):-
	!. %red cut, avoid trying other possibilities
split_slot(Day,Start,End,Start,Stop,Rest,NewSlots):-
	Post is Stop - End,
	insert_slot(slot(Day,End,Post),Rest,NewSlots),
	!. %red cut, avoid trying other possibilities
split_slot(Day,Hour,Stop,Start,Stop,Rest,NewSlots):-
	Pre is Hour - Start,
	insert_slot(slot(Day,Start,Pre),Rest,NewSlots),
	!. %red cut, avoid trying other possibilities
split_slot(Day,Hour,End,Start,Stop,Rest,NewSlots):-
	Pre is Hour - Start,
	Post is Stop - End,
	insert_slot(slot(Day,Start,Pre),Rest,TempNew),
	insert_slot(slot(Day,End,Post),TempNew,NewSlots).

slot_sm(slot(_,_,D1),slot(_,_,D2)):-
	D1 < D2.

insert_slot(S,[],[S]).
insert_slot(S,[H|T],[H|Z]):-
	slot_sm(S,H),
	!, %red cut
	insert_slot(S,T,Z).
insert_slot(S,[H|T],[S,H|T]).

%is_valid(Schedule) <- is Schedule a valid exam schedule.

conflict(E1,E2):-
	follows(S,E1),
	follows(S,E2).

conflict(E1,E2):-
	has_exam(C1,E1),
	has_exam(C2,E2),
	teaches(L,C1),
	teaches(L,C2).

reservation([],Event,[Event]).
reservation([H|S],E,[H|Z]):-
	before(H,E),
	reservation(S,E,Z).
reservation([]):-


	


is_valid(schedule(EventList)):-
	full_schedule(EventList),
	available_timeslots(FreeSlots),
	is_valid(EventList,FreeSlots,[]).

is_valid([],_,_).
is_valid([Event|Events],FreeSlots,Reserved):-
	free_slot(FreeSlots,Event,NewFreeSlots),
	reservation(Reserved,Event,NewReservations),
	is_valid(Events,NewFreeSlots,NewReservations).