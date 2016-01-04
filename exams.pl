%%% MODULE EXAMS %%%

/**
	This module contains general-purpose predicates that are related to exams and their schedules.
	They are often required by many of the other predicates and hence factored out into this separate module.
*/

:- module(exams,
	[exam_lecturer/2,
	 takes_exam/2,
	 room_suitable/2,
	 room_available/5,
	 schedule/2,
	 mutual_exclusive/2,
	 conflict/2]).


%%% IMPORTS %%%

:- use_module(setup, [required_capacity/2, share_students/3]).
:- use_module(utils, [overlap/4]).


%%% PERSON - EXAM RELATIONS %%%

%% exam_lecturer(?Exam,?Lecturer)
%%     Exam is the exam of a course that is given by Lecturer.
%%     Can be used in both ways to either get 
%%			* the lecturer of an exam
%%			* an exam of a lecturer
%% 	   The implementation is optimized to efficiently handle both cases.
exam_lecturer(Exam,Lecturer):-
	nonvar(Exam) -> %optimization!
		(has_exam(Course,Exam),
		 teaches(Lecturer,Course));
		(teaches(Lecturer,Course),
		 has_exam(Course,Exam)).

%% takes_exam(?Student,?Exam)
%%     Exam is taken by Student.
%%     Can be used in both ways to either get 
%%			* the students of an exam
%%			* an exams of a student
%% 	   The implementation is optimized to efficiently handle both cases.
takes_exam(Student,Exam):-
	nonvar(Exam) -> %optimization!
		(has_exam(Course,Exam),
		 follows(Student,Course));
		(follows(Student,Course),
		 has_exam(Course,Exam)).


%%% ROOMS FOR EXAMS %%%

%% room_suitable(?Exam,?Room)
%%     Room has sufficient capacity for Exam.
%%	   Exam and/or Room can both be uninstantiated,
%%     for instance to find all suitable rooms for an exam.
room_suitable(Exam,Room):-
	required_capacity(Exam,ReqCapacity),
	capacity(Room,Capacity),
	ReqCapacity =< Capacity.

%% room_available(+Exam,+Room,?Day,?Hour,?End).
%%     Assuming that Room has the required capacity for Exam,
%%     Exam could be held in Room on Day between Hour and End
room_available(Exam,Room,Day,Hour,End):-
	duration(Exam,Duration),
	availability(Room,Day,Start,Stop),
	LatestStart is Stop - Duration,
	catch(between(Start,LatestStart,Hour),_,fail), %throws an exception when exam does not start on an hour (i.e. when Hour is a float)
	End is Hour + Duration.


%%% SCHEDULE FORMATS %%%

%% schedule(?ExamList,?EventList)
%%     Allows to convert between the internal schedule format and the one specified in the assignment.
%%	   Both are nearly identical, the only difference is that an internal ExamList also keeps the ending hour for each scheduled exam in EventList
%% 	   The conversion can be done in both ways by Instantiating either one of the arguments.
schedule([],[]).
schedule([exam(E,R,D,H,F)|Exams],[event(E,R,D,H)|Events]):-
	schedule(Exams,Events),
	duration(E,Duration),
	F is H + Duration.


%%% CONFLICTS BETWEEN EXAMS %%%

%% mutual_exclusive(?E1,?E2)
%%     Checks if two exams are mutual exclusive, 
%%	   i.e. if they can be held at the same time or not.
%%	   This is not the case when either:
%%			* both exams share the same lecturer
%%			* both exams share any students
mutual_exclusive(E1,E2):-
	exam_lecturer(E1,L),
	exam_lecturer(E2,L).
mutual_exclusive(E1,E2):-
	share_students(E1,E2,_).

%% conflict(+Exam,+ExamSchedule)
%%     checks if a scheduled Exam cause a conflict with an existing ExamSchedule.
%%	   this is the case if it overlaps with any of the other exams and either:
%%			* shares the same room as that exam
%%			* is defined as mutual exclusive (cf. sup.) w.r.t. the other exam	   
conflict(exam(E1,R1,D,H1,F1),[exam(E2,R2,D,H2,F2)|_]):-
	overlap(H1,F1,H2,F2),
	(R1 = R2 ; mutual_exclusive(E1,E2)).
conflict(NewExam,[_|Exams]):-
	conflict(NewExam,Exams).