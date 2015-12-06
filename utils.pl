:- module(utils,
	[remove_one/3,
	 overlap/4,
	 member/2,
	 max/3,
	 min/3,
	 take/4]).


%%% UTILS %%%

remove_one([E|R],E,R).
remove_one([H|T],E,[H|Z]):-
	H \= E,
	remove_one(T,E,Z).

overlap(H1,F1,H2,_):- 
	H1 =< H2, 
	F1 > H2.
overlap(H1,_,H2,F2):- 
	H1 > H2,
	H1 < F2.

member(E,[E|_]).
member(E,[_|T]):-
	member(E,T).

max(M,N,M):- M>=N,!.
max(M,N,N):- M<N.

min(M,N,M):- M=<N,!.
min(M,N,N):- M>N.

%result of first part is difference list
take(X,0,A-A,X):-!.
take([],_,A-A,[]):-!.
take([H|T],N,[H|Z]-A,R):-
	N > 0,
	N1 is N - 1,
	take(T,N1,Z-A,R).


