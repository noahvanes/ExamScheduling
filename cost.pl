%%% MODULE COST %%%

/**
	The predicate exported in this module can calculate the cost of an exam schedule.
	It works by first listing violated constraints through violates_sc and accumulating their costs.
*/

:- module(cost,
	[cost/2,
	 exams_cost/2]).


%%% IMPORTS %%%

:- use_module(violates_sc, [violates_sc/2,exams_violate_sc/2,person/2,penalty/2]).
:- use_module(setup, [student_count/1,lecturer_count/1]).


%%% CALCULATING SCHEDULE COST %%%

%% cost(+Schedule,-Cost)
%%     Cost is the normalized cost of Schedule
%%     This is calculated as specified in the assignment       
cost(Schedule,Cost):-
	violates_sc(Schedule,Constraints),
	constraint_costs(Constraints,Cost).

%% exams_cost(+ExamList,-Cost)
%%     Identical to cost/2, but for internal usage in other modules
%%     Assumes that memoized data is already setup, and uses the internal schedule format
exams_cost(Schedule,Cost):-
	exams_violate_sc(Schedule,Constraints),
	constraint_costs(Constraints,Cost).


%%% CALCULATING CONSTRAINTS COST %%%

%% constraint_costs(+Contraints,-Cost)
%%      Calculates the normalized Cost from a list of Contraints
constraint_costs(Constraints,Cost):-
	student_count(AmountOfStudents),
	lecturer_count(AmountOfLecturers),
	constraint_costs(Constraints,0,0,SC,LC),
	AvgStudentCost is SC/AmountOfStudents,
	AvgLecturerCost is LC/AmountOfLecturers,
	Cost is (AvgStudentCost + AvgLecturerCost)/2.
	
%% constraint_costs(+Constraints,+CurrentSC,+CurrentLC,-SC,-LC)
%%     Contraints is a list of Constraints
%%     CurrentSC is the currently accumulated cost for students
%%	   CurrentLC is the currently accumulated cost for lecturers
%%	   SC and LC are the total costs for students and lecturers, respectively
%%     (this iteration is used internally for constraint_costs/2)
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