# anti_unify
Relational anti-unification for SWI-Prolog

`anti_unify/3` is intended as a relational drop-in replacement for [`term_subsumer/3`](https://www.swi-prolog.org/pldoc/doc_for?object=term_subsumer/3). It can handle cyclic data, as well as cases where cyclic data would be induced.

```prolog
?- anti_unify(X, Y, f(P, Q)).
X = f(_XP, _XQ),
Y = f(_YP, _YQ),
anti_unify(_XP, _YP, P),
maplist(subsumes(P), [_XP, _YP]),
anti_unify(_XQ, _YQ, Q),
maplist(subsumes(Q), [_XQ, _YQ]).

?- anti_unify(X, Y, Z), Z = f(Y). % Example with induced cyclic data.
X = Y, Y = Z, Z = f(Z).

?- anti_unify(X, Y, Z), Z = g(_).
X = g(_A),
Y = g(_B),
Z = g(_C),
anti_unify(_A, _B, _C),
maplist(subsumes(_C), [_A, _B]).
```

See the unit tests in [`test/anti_unify.plt`](test/anti_unify.plt) for more examples.

Executing the following goal from the top-level `subsumes` directory should run all the tests:
```prolog
?- expand_file_name("test/**.plt", Tests), maplist(consult, Tests), run_tests.
```

TODO: make ISO-compatible.

(Note to self) To publish a new version:
1. update `pack.pl`
2. do GitHub release with new tag matching the pack.pl version
3. execute:
```prolog
?- make_directory(potato), pack_install(anti_unify, [url('http://github.com/GeoffChurch/anti_unify/archive/13.17.zip'), package_directory(potato)]).
```
