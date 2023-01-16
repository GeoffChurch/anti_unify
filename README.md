# anti_unify
Relational anti-unification for SWI-Prolog

`anti_unify/3` is intended as a relational drop-in replacement for [`term_subsumer/3`](https://www.swi-prolog.org/pldoc/doc_for?object=term_subsumer/3). It can handle cyclic data, as well as cases where cyclic data would be induced.

```prolog
?- anti_unify(X, Y, f(A, B)).
X = f(_53014, _53016),
Y = f(_53032, _53034),
maplist(subsumes(A), [_53032, _53014]),
maplist(subsumes(B), [_53034, _53016]),
when((nonvar(_53014), nonvar(_53032)), anti_unify:myguardedmap(_53014, _53032, A)),
when((nonvar(_53016), nonvar(_53034)), anti_unify:myguardedmap(_53016, _53034, B)).

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
