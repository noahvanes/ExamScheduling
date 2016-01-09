%%% MODULE FIND_OPTIMAL %%%

/**
	Used to generate and check for optimal schedules.
	Only applicable to smaller datasets, use the approximating find_heuristically in larger ones.
*/

:- module(optimal,
	[is_optimal/1,
	 find_optimal/1]).


%%% IMPORTS %%%

:- use_module(is_valid, [is_valid/1]).
:- use_module(cost, [exams_cost/2]).
:- use_module(exams, [schedule/2]).
:- use_module(utils, [member/2,permutation/2]).


%%% OPTIMAL SCHEDULES %%%

%% is_optimal(?Schedule)
%%      Schedule is an optimal schedule (i.e. minimizes cost)
%%      Can be used to either:
%%          * check if a given schedule is optimal
%%			* find all optimal schedules exactly once
is_optimal(schedule(X)):-
	findall((S,C),
			(is_valid(schedule(S)),
			 schedule(Exams,S),
			 exams_cost(Exams,C)),
			Schedules),
	optimal_schedules(Schedules,Optimals),
	member(OptimalSchedule,Optimals),
	equivalent(OptimalSchedule,X).

%% find_optimal(-Schedule)
%%     (Added for the sake of completeness) 
%%     Outputs a single optimal schedule
find_optimal(X):-
	is_optimal(X),
	!. %red cut <- only output one optimal schedule


%%% EQUIVALENT SCHEDULES %%%

%% equivalent(+Schedule,?OtherSchedule)
%%     OtherSchedule is equivalent to Schedule
%%     i.e. they share the same events, possibly permutated
equivalent(Schedule,OtherSchedule):-
	permutation(Schedule,OtherSchedule),
	!. %red cut <- only looking for one equivalent permutation


%%% FILTERING OUT OPTIMAL SCHEDULES %%%

%% optimal_schedules(+Schedules,-Optimals)
%%     Optimals are the best schedules from a list of Schedules and their costs
optimal_schedules([(Schedule,Cost)|Schedules],Optimals):-
	optimal_schedules(Schedules,Cost,[Schedule],Optimals).
	
%% optimal_schedules(+Schedules,+BestCost,+BestSchedules,-Optimals)
%%     Schedules in a list of (schedule,cost)
%%     BestCost is the best cost encountered so far
%%     BestSchedules are the best schedules encountered so far
%%     Optimals are the (globally) best schedules
%%     (this iteration is used internally by optimal_schedules/2) 
optimal_schedules([],_,Optimals,Optimals).
optimal_schedules([(S,C)|Schedules],C,Current,Optimals):-
	!, %green cut <- cost is the same, no need to backtrack
	optimal_schedules(Schedules,C,[S|Current],Optimals).
optimal_schedules([(_,C)|Schedules],Cost,Current,Optimals):-
	Cost < C,
	!, %green cut <- cost is higher, no need to backtrack
	optimal_schedules(Schedules,Cost,Current,Optimals).
optimal_schedules([(S,C)|Schedules],Cost,_,Optimals):-
	C < Cost,
	!, %green cut <- cost is lower, no need to backtrack
	optimal_schedules(Schedules,C,[S],Optimals).