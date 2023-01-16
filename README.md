# anti_unify
Relational anti-unification for SWI-Prolog

`anti_unify/3` is intended as a relational drop-in replacement for [`term_subsumer/3`](https://www.swi-prolog.org/pldoc/doc_for?object=term_subsumer/3). It can handle cyclic data, as well as cases where cyclic data would be induced.

```prolog
?- anti_unify(X, Y, f(P, Q)).
X = f(_XP, _XQ),
Y = f(_YP, _YQ),
maplist(subsumes(P), [_YP, _XP]),
maplist(subsumes(Q), [_YQ, _XQ]),
when((nonvar(_XP), nonvar(_YP)), anti_unify:myguardedmap(_XP, _YP, P)),
when((nonvar(_XQ), nonvar(_YQ)), anti_unify:myguardedmap(_XQ, _YQ, Q)).

?- anti_unify(X, Y, Z), Z = f(Y). % Example with induced cyclic data.
X = Y, Y = Z, Z = f(Z).

?- anti_unify(X, Y, Z), Z = g(_).
X = g(_A),
Y = g(_B),
Z = g(_C),
maplist(subsumes(_C), [_B, _A]),
when((nonvar(_A), nonvar(_B)), anti_unify:myguardedmap(_A, _B, _C)).
```

See the unit tests in [`prolog/anti_unify.plt`](prolog/anti_unify.plt) for more examples.

TODO: Rather than relying on `subsumes` and `when`, maybe do all the attrvar stuff locally to handle identity detection in the case of unifying two attrvar antiunificands (see `test(indirect_identity_inferred_var_fails)` in the unit tests).

TODO: make ISO-compatible.
