:- module(anti_unify, [anti_unify/3]).

:- use_module(subsumes, [subsumes/2]).
:- use_module(guardedmap, [guardedmap/3]).

anti_unify(A, B, LGG) :-
    % It's cleaner to assert subsumption up front,
    % even though it traverses LGG more than necessary.
    subsumes(LGG, A),
    subsumes(LGG, B),
    myguardedmap(A, B, LGG).

% anti_unify(A, B, LGG) assumes that guard(A, B, LGG) has just succeeded.
anti_unify_(A, B, LGG) :-
    % If A == B then it is its own LGG.
    A == B, !, LGG = A.
anti_unify_(A, B, LGG) :-
    % anti_unify(A, LGG, LGG) iff LGG subsumes A, which is already
    % enforced, so the "when" clause is superfluous.
    (LGG == A ; LGG == B), !.
anti_unify_(A, B, _LGG) :-
    % If A and B are both nonvar then guard(A, B, LGG) implies that they
    % have different functors, so LGG is permavar (can never be nonvar),
    % which characterizes its observable behavior and is already enforced
    % by its existing subsumption of A and B.
    nonvar(A), nonvar(B), !.
anti_unify_(A, B, LGG) :-
    when((nonvar(A), nonvar(B)),
         myguardedmap(A, B, LGG)).

myguardedmap(A, B, LGG) :-
    guardedmap(guard, anti_unify_, [A, B, LGG]).

guard(A, B, _LGG) :-
    once(A == B ;
         var(A) ;
         var(B) ;
         \+ same_functor(A, B)).

%%% UTILS %%%

same_functor(A, B) :-
    functor(A, Name, Arity),
    functor(B, Name, Arity).
