:- use_module(subsumes).

:- begin_tests(subsumes).

test(direct_unresolvable_subsumption_fails) :-
    \+ f subsumes g.

test(indirect_unresolvable_subsumption_fails) :-
    X subsumes f,
    X subsumes g,
    \+ g subsumes X.

test(idempotent) :-
    maplist(subsumes(X), [f(g),f(g),A,B,A]),
    check_lbs(X, [A,B,f(g)]).

test(multiple_cycles_collapsed) :-
    X = [A,B,C,A,D],
    Y = [B,C,E,D,E],
    maplist([V]>>(V subsumes f(V)), X),
    X subsumes Y,
    % Now induce cycle with two distinct paths: A-B-C-E and A-D-E.
    E subsumes A,
    % Make sure both paths were collapsed.
    maplist(==(A), [B,C,D,E]),
    check_lbs(A, [f(A)]).

test(permavar_lbs_compressed) :-
    maplist(subsumes(X), [1, 2, 3, 4, 5, 6]),
    check_lbs(X, [every, thing]).

test(subsumption_with_induced_cyclic_data_terminates) :-
    f(X, Y) subsumes X,
    X == f(X, Y).

test(subsumption_with_cyclic_data) :-
    X = f(X, Y),
    X subsumes f(f(A, B), C),
    term_variables(A, [Fresh]),
    A == f(A, Fresh),    
    check_lbs(Y, [C, B, Fresh]).

test(subsume_then_unify_leaves_loop) :-
    X subsumes Y,
    X = Y,
    % This is unfortunate but unavoidable, as attr_unify_hook is not called
    % when unifying with an unattributed variable. However, it is unobservable
    % because compact_lbs/1 is called by attribute_goals//1.
    check_lbs(X, [X]).

:- end_tests(subsumes).

%%% UTILS %%%

get_lbs(G, LBs) :- get_attr(G, subsumes, LBs), !.
get_lbs(_, []).

check_lbs(G, Expected) :-
    get_lbs(G, Actual),
    msort(Expected, X),
    msort(Actual, X).
