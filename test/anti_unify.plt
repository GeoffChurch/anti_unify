:- use_module("../prolog/anti_unify").
:- use_module(library(apply), [maplist/2, maplist/3]).

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

test(subsumption_holds) :-
    anti_unify(X, _, Z),
    X = 3,
    Z \= 4,
    var(Z),
    Z = 3.

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

test(var_sharing) :-
    anti_unify(X, Y, Z),
    X = f(A, A),
    Y = f(B, B),
    nat_variant(Z, f(C, C)).

test(nested_var_sharing) :-
    anti_unify(X, Y, Z),
    X = f(g(A, A)),
    Y = f(g(B, B)),
    nat_variant(Z, f(g(C, C))).

test(mixed_sharing) :-
    anti_unify(X, Y, Z),
    X = f(A, _, A),
    Y = f(C, _, C),
    nat_variant(Z, f(P, _, P)).

test(forced_unshare) :-
    anti_unify(X, Y, Z),
    X = f(A, A),
    Y = f(_, _),
    nat_variant(Z, f(_, _)).

test(equal_atoms) :-
    anti_unify(a, a, Z),
    Z == a.

test(functor_mismatch_yields_permavar) :-
    anti_unify(X, Y, Z),
    X = f(a), Y = g(a),
    % Z is permavar
    var(Z),
    Z \= f(_).

test(anti_unification_composes) :-
    anti_unify(A, B, X),
    anti_unify(X, C, Y),
    A = f(a, 1),
    B = f(a, 2),
    C = f(a, _),
    nat_variant(Y, f(a, _)),
    Y = f(a, V),
    % V is permavar
    var(V),
    V \= 1.

test(residuals_exist) :-
    anti_unify(X, Y, Z1),
    anti_unify(X, Y, Z2),
    msort([myguardedmap(X, Y, Z1), myguardedmap(X, Y, Z2)], ExpectedCallbacks),
    get_attr(X, anti_unify, XCallbacks),
    msort(XCallbacks, XActual),
    XActual == ExpectedCallbacks,
    get_attr(Y, anti_unify, YCallbacks),
    msort(YCallbacks, YActual),
    YActual == ExpectedCallbacks,
    copy_term((X, Y, Z1, Z2), (X_, Y_, Z1_, Z2_), Goals),
    msort(Goals, Actual),
    msort([
        anti_unify(X_, Y_, Z1_),
        anti_unify(X_, Y_, Z2_),
        maplist(subsumes:subsumes(Z1_), [X_, Y_]),
        maplist(subsumes:subsumes(Z2_), [X_, Y_])
    ], Expected),
    Actual == Expected.

test(residuals_work) :-
    anti_unify(X, Y, Z),
    X = f(_),
    copy_term((X, Y, Z), (_, Y_, Z_), Goals),
    maplist(call, Goals),
    Y_ = f(2),
    nat_variant(Z_, f(_)).

:- end_tests(anti_unify).

%%% UTILS %%%

nat_variant(X, Y) :-
    maplist(copy_term_nat, [X, Y], [X_, Y_]),
    X_ =@= Y_.
