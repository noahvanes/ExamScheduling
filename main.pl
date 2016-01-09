%%% MAIN %%%

/**
	This file simply imports all relevant predicates from the assignment,
	so that the user doesn't have to selectively import from each module.
*/


%%% IMPORTS %%%

:- use_module(is_valid, [is_valid/1]).
:- use_module(cost, [cost/2]).
:- use_module(violates_sc, [violates_sc/2]).
:- use_module(optimal, [find_optimal/1,is_optimal/1]).
:- use_module(heuristic_search, [find_heuristically/1,find_heuristically/2]).
:- use_module(pretty, [pretty_print/1,pretty_print/2]).