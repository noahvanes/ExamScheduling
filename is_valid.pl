%%% MODULE IS_VALID %%%

/**
	This module implements the is_valid predicate, as specified in the assignment.
	It is also able to generate all schedules exactly once, as well as check for a valid schedule in any order.
*/

:- module(is_valid, 
	[is_valid/1]).


%%% IMPORTS %%%

:- use_module(setup, [setup_assertions/0,exams/1]).
:- use_module(exams, [room_suitable/2,room_available/5,conflict/2]).
:- use_module(utils, [remove_one/3]).


%%% IS_VALID %%%

%% is_valid(?Schedule)
%%     Schedule is a valid schedule of exams
%%	   Can be used to either:
%%			* check the validity of a schedule (with events in any order)
%%			* generate a schedule (will eventually generate all schedules exactly once)
is_valid(schedule(EventList)):-
	setup_assertions,
	exams(FullExamList),
	is_valid(EventList,FullExamList,[]).

%% is_valid(?EventList,+Exams,+Reservations)
%%     EventList schedules all Exams without any conflict to existing Reservations
%%	   Also checks individual exam requirements such as room capacity and availability
%%	   (not exported; private to this module for usage in is_valid/1)
is_valid([],[],_).
is_valid([event(Exam,Room,Day,Hour)|Events],Exams,Reservations):-
	remove_one(Exams,Exam,Remaining),
	!, %red cut <- consider only one exam each time to avoid duplicates
	room_suitable(Exam,Room),
	room_available(Exam,Room,Day,Hour,End),
	NewEntry = exam(Exam,Room,Day,Hour,End),
	not(conflict(NewEntry,Reservations)),
	is_valid(Events,Remaining,[NewEntry|Reservations]).