%% UTILS

delete_from([E|T],E,T).
delete_from([H|T],E,[H|Z]):-
	H \= E,
	delete_from(T,E,Z).

difference_list([],Y-Y).
difference_list([H|T],[H|Y]-Y0):-
	difference_list(T,Y-Y0).

merge([H1|T1],[H2|T2],[H1|Z],P):-
	P(H1,
	

% state(currentSchedule,toSchedule)

%% SCHEDULE REPRESENTATION + ABSTRACTION

%	[ R1, R2, R3, ..., RN ]
%	   \---\---\--------\--> [(D,START,DURATION)|...]
%			\---\--------\--> [...]
%				 \--------\--> [...]
%						   \--> [...]

empty_schedule(X):-
	findall(room(R,Availabilities),
			bagof(free_slot(D,St,Sp),availability(R,D,St,Sp),Availabilities),
			X).

my_insert(Exam,

%% SEARCH PROBLEM

goal(s(FinalSchedule,[])).

successor(s(Schedule,Exams),s(NewSchedule,Rest)):-
	delete_from(Exams,E,Rest),
	!, %red cut, only consider one for CSP
	insert_schedule(Schedule,E,NewSchedule).

is_valid(Schedule):-
	empty_schedule(E),
	findall(E,exam(E,_),Exams),
	my_schedule(Schedule,MySchedule),
	search(s(E,Exams),s(MySchedule,[])).

search(Goal,Goal):- 
	goal(Goal).
search(Current,Goal):-
	successor(Current,NewState),
	search(NewState,Goal).
