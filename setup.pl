%%% MODULE SETUP %%%

/**
	This module provides extra static information that can be deduced from the datasets.
	The idea is that many facts here are only calculated once and then reused in other predicates.
	Therefore, its main purpose is that of efficiency...
*/

:- module(setup,
	[setup_assertions/0,
	 exams/1,
	 share_students/3,
	 required_capacity/2,
	 students_exams/2,
	 student_count/1,
	 lecturer_count/1]).

:- dynamic 
	setup_completed/0,
	share_students/3,
	required_capacity/2,
	students_exams/2,
	student_count/1,
	lecturer_count/1,
	exams/1.


%%% IMPORTS %%%

:- use_module(exams, [takes_exam/2]).


%%% SETUP %%%

%% setup_assertions
%%     Used to setup memoized data for a loaded instance.
%%     Employs a safety check to unload old instance data first
setup_assertions:-
	setup_completed,
	retract_assertions,
	setup_assertions.
setup_assertions:-
	not(setup_completed),
	assert_all_exams,
	assert_student_count,
	assert_lecturer_count,
	findall(_,assert_exam_capacity,_),
	findall(_,assert_shared_exam_students,_),
	findall(_,assert_students_exams,_),
	asserta(setup_completed).


%%% CLEAN-UP %%%

%% retract_assertions
%%     used to unload memoized data of a loaded instance
retract_assertions:-
	retractall(exams(_)),
	retractall(share_students(_,_,_)),
	retractall(required_capacity(_,_)),
	retractall(students_exams(_,_)),
	retractall(student_count(_)),
	retractall(lecturer_count(_)),
	retractall(setup_completed).


%%% HELPER PREDICATES %%%

%% takes_exams(?Student,?Exam1,?Exam2)
%%     Student takes both Exam1 and Exam2
%%	   Instantiation of arguments can be arbitrarily chosen
takes_exams(S,E1,E2):-
	takes_exam(S,E1),
	takes_exam(S,E2),
	E1 \= E2.

%% assert_shared_exam_students
%%     Registers for two exams that share students the list of shared students
assert_shared_exam_students:-
	bagof(S,takes_exams(S,E1,E2),L),
	asserta(share_students(E1,E2,L)).

%% assert_exam_capacity
%%     Registers how much capacity (in a room) is required for an exam
assert_exam_capacity:-
	bagof(S,takes_exam(S,E),L),
	length(L,N),
	asserta(required_capacity(E,N)).

%% assert_students_exams
%%     Based on the observations that many students share exactly the same exam schedule,
%%     This predicate registers lists of students with the lists of exams that they all take
assert_students_exams:-
	bagof(Student,
		  N^(student(Student,N),
		  	 findall(E,takes_exam(Student,E),Exams)),
		  Students),
	asserta(students_exams(Students,Exams)).

%% assert_all_exams
%%     This predicates simply registers all of the exams into a single list
assert_all_exams:-
	findall(E,exam(E,_),Exams),
	asserta(exams(Exams)).

%% assert_student_count
%%     Calculates beforehand the total number of students
assert_student_count:-
	findall(_,student(_,_),L),
	length(L,N),
	asserta(student_count(N)).

%% assert_lecturer_count
%%     Calculates beforehand the total number of lecturers
assert_lecturer_count:-
	findall(_,lecturer(_,_),L),
	length(L,N),
	asserta(lecturer_count(N)).