:- module(kb_assertions,
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


%%% RUN-TIME ASSERTIONS %%%

assert_shared_exam_students:-
	bagof(S,takes_exams(S,E1,E2),L),
	asserta(share_students(E1,E2,L)).

assert_exam_capacity:-
	bagof(S,takes_exam(S,E),L),
	length(L,N),
	asserta(required_capacity(E,N)).

assert_students_exams:-
	bagof(Student,
		  N^(student(Student,N),
		  	 findall(E,takes_exam(Student,E),Exams)),
		  Students),
	asserta(students_exams(Students,Exams)).

assert_all_exams:-
	findall(E,exam(E,_),Exams),
	asserta(exams(Exams)).

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
	assert_all_exams,
	assert_student_count,
	assert_lecturer_count,
	findall(_,assert_exam_capacity,_),
	findall(_,assert_shared_exam_students,_),
	findall(_,assert_students_exams,_),
	asserta(setup_completed).