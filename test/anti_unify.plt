:- use_module("../prolog/anti_unify").

:- begin_tests(anti_unify).

test(lgg_pushes_down_antiunificands) :-
    anti_unify(X, Y, Z),
    Z = f(_, _),
    nat_variant(X, f(_, _)),
    nat_variant(X, Y),
    X \== Y.

test(antiunificands_drag_down_lgg) :-
    anti_unify(X, Y, Z),
    X = f(g(h(_)), 4),
    Y = f(g(_), 4),
    nat_variant(Y, Z),
    Y \== Z.

test(unresolvable_antiunification_fails) :-
    anti_unify(X, _, Z),
    X = 3,
    Z \= 4.

test(cyclic_data) :-
    X = f(X),
    anti_unify(A, B, X),
    A == X,
    B == X.

test(induced_cyclic_data) :-
    anti_unify(X, Y, Z),
    Z = f(Y),
    Y == f(Y),
    X == Y.

test(direct_identity_inferred, [X == Z]) :-
    anti_unify(X, X, Z).

test(indirect_identity_inferred_var, [X == Z]) :-
    anti_unify(X, Y, Z),
    X = Y.
    
test(indirect_identity_inferred_nonvar, [X == Z]) :-
    anti_unify(X, Y, Z),
    X = Y,
    X = f(_).

test(var_multiple_occurrences, [Z =@= f(C, C)]) :-
    anti_unify(X, Y, Z),
    X = f(A, A),
    Y = f(B, B).

:- end_tests(anti_unify).

%%% UTILS %%%

nat_variant(X, Y) :-
    maplist(copy_term_nat, [X, Y], [X_, Y_]),
    X_ =@= Y_.
