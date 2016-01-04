%%% MODULE PRETTY PRINT %%%

/**
	This module contains predicates that allow pretty printing of exam schedules.
	There also a is variant available to print a personalized exam schedule for a student.
*/

:- module(pretty,
	[pretty_print/1,
	 pretty_print/2]).


%%% IMPORTS %%%

:- use_module(utils, [member/2]).
:- use_module(exams, [exam_lecturer/2,takes_exam/2]).


%%% GENERAL PRETTY PRINT %%%

%% pretty_print(+Schedule)
%%     prints out Schedule as specified in the assignment
pretty_print(schedule(Schedule)):-
	findall(_,pretty(Schedule),_),
	nl. %put newline after schedule


%%% PERSONALIZED EXAM SCHEDULES %%%

%% pretty_print(+Schedule,+SID)
%%     prints out the exam Schedule for student SID
pretty_print(schedule(Schedule),SID):-
	student_filter(SID,Schedule,StudentSchedule),
	pretty_print(schedule(StudentSchedule)).

%% student_filter(+SID,+Events,-FilteredEvents)
%%      FilteredEvents is Events, filtered to contain only the exams of student SID
student_filter(_,[],[]).
student_filter(SID,[event(E,_,_,_)|T],Z):-
	not(takes_exam(SID,E)),
	!,
	student_filter(SID,T,Z).
student_filter(SID,[E|T],[E|Z]):-
	student_filter(SID,T,Z).


%%% PRINTER TRAVERSAL %%%

%% pretty(+EventList)
%%     EventList is a list of scheduled events
%%     This predicate views these events as a tree, and prints the next event
%%     The native prolog stack is then used to backtrack to previous days and rooms
pretty(EventList):-
	day_schedule(EventList,Day,DaySchedule),
	pretty_print_day(Day),
	room_schedule(DaySchedule,Room,RoomSchedule),
	pretty_print_room(Room),
	exam_entry(RoomSchedule,Hour,Exam),
	pretty_print_exam(Hour,Exam).

%% day_schedule(+EventList,?Day,-DaySchedule)
%%     DaySchedule is the EventList filtered for a specific Day
%%     (used by pretty/1 to traverse all scheduled days chronologically)
day_schedule(EventList,Day,DaySchedule):-
	findall((D,S),
			bagof((E,R,H),member(event(E,R,D,H),EventList),S),
			UnsortedSchedules),
	sort(1,@=<,UnsortedSchedules,Schedules),
	member((Day,DaySchedule),Schedules).

%% room_schedule(+DaySchedule,?Room,-RoomSchedule)
%%     RoomSchedule is the DaySchedule filtered for a specific Room
%%     (used by pretty/1 to traverse all scheduled rooms)
room_schedule(DaySchedule,Room,RoomSchedule):-
	bagof((E,H),
		  member((E,Room,H),DaySchedule),
		  RoomSchedule).

%% exam_entry(+RoomSchedule,?Hour,?Exam)
%%     Exam is scheduled at Hour in a given RoomSchedule
%%     (used by pretty/1 to traverse all scheduled exams chronologically)
exam_entry(RoomSchedule,Hour,Exam):-
	sort(2,@<,RoomSchedule,SortedSlots),
	member((Exam,Hour),SortedSlots).

	
%%% PRETTY PRINTING FOR DAYS/ROOMS/EXAMS %%%

%% pretty_print_day(+Day)
%%     Pretty prints a heading for Day
pretty_print_day(Day):-
	nl,
	write('*** DAY '),
	write(Day),
	write(' ***'),
	nl.

%% pretty_print_room(+Room)
%%     Pretty prints a heading for Room
pretty_print_room(Room):-
	nl,
	room(Room,RoomName),
	write(RoomName),
	nl.

%% pretty_print_exam(+Hour,+Exam)
%%     Pretty prints the entry for Exam, scheduled at Hour
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