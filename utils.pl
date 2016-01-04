%%% MODULE UTILS %%%

/**
	This module contains general-purpose predicates that are not domain-specific.
	They are factored out into this module because they frequently appear in other predicates.
*/

:- module(utils,
	[remove_one/3,
	 member/2,
	 take/4,
	 overlap/4,
	 max/3,
	 min/3]).


%%% LIST OPERATIONS %%%

%% remove_one(?List,?Element,?Remaining)
%%     Remaining is List with one instance of Element removed
%%	   Can be used in multiple directions
remove_one([E|R],E,R).
remove_one([H|T],E,[H|Z]):-
	remove_one(T,E,Z).

%% member(?Element,?List)
%%     Element is a member of List
%%	   Can also be used in multiple directions,
%%	   for instance to go over all elements of a list
member(E,[E|_]).
member(E,[_|T]):-
	member(E,T).

%% take(?List,+N,?Take,?Rest)
%%     List is split by having the first N elements in Take, the rest in Rest
take(X,0,[],X).
take([],N,[],[]):-
	N > 0. %avoid multiple matches
take([H|T],N,[H|Z],R):-
	N > 0,
	N1 is N - 1,
	take(T,N1,Z,R).


%%% NUMERICAL UTILITIES %%%

%% overlap(+H1,+F1,+H2,+F2)
%%     Checks if two intervals (H1,F1) and (H2,F2) overlap
overlap(H1,F1,H2,_):- 
	H1 =< H2, 
	F1 > H2.
overlap(H1,_,H2,F2):- 
	H1 > H2,
	H1 < F2.

%% max(+M,+N,?X)
%%     X is the maximum of M and N
max(M,N,M):- M>=N,!. %green cut
max(M,N,N):- M<N.

%% min(+M,+N,?X)
%%     X is the minimum of M and N
min(M,N,M):- M=<N,!. %green cut
min(M,N,N):- M>N.



