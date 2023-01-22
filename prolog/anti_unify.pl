:- module(anti_unify, [anti_unify/3]).

:- use_module(library(subsumes)).
:- consult(guardedmap).

%!  anti_unify(?A, ?B, ?LGG) is semidet.
%
%   anti_unify/3 maintains the relation that `LGG` is the least general
%   generalization of `A` and `B`.
%
%   See the unit tests for examples.
anti_unify(A, B, LGG) :-
    % It's cleaner to assert subsumption up front,
    % even though it traverses LGG more than necessary.
    LGG subsumes A,
    LGG subsumes B,
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
    Callback = myguardedmap(A, B, LGG),
    (var(A)  ->  add_callback(A, Callback) ; true),
    (var(B)  ->  add_callback(B, Callback) ; true).

myguardedmap(A, B, LGG) :- guardedmap(guard, anti_unify_, [A, B, LGG]).

guard(A, B, _LGG) :-
    once(A == B ;
         var(A) ;
         var(B) ;
         \+ same_functor(A, B)).

get_callbacks(Var, Callbacks) :- get_attr(Var, anti_unify, Callbacks), !.
get_callbacks(_, []).

set_callbacks(Var, []) :- !, del_attr(Var, anti_unify).
set_callbacks(Var, Callbacks) :- put_attr(Var, anti_unify, Callbacks).

add_callback(Var, Callback) :-
    get_callbacks(Var, Callbacks),
    set_callbacks(Var, [Callback|Callbacks]).

attr_unify_hook(XCallbacks, Y) :-
    % Call it all!
    maplist(call, XCallbacks),
    (var(Y)
    ->  get_callbacks(Y, YCallbacks),
	set_callbacks(Y, []),
	maplist(call, YCallbacks)
    ;   true).

attribute_goals(V) -->
    { call_dcg(
	  (get_callbacks, maplist(private_public), include(is_first_antiunificand(V))),
	  V, Goals) },
    Goals.

% The callbacks use the non-exported myguardedmap/3 as a slight optimization,
% but for attribute_goals//1 we replace it with the exported anti_unify/3.
private_public(myguardedmap(A, B, LGG), anti_unify(A, B, LGG)).

% Each antiunificand has a copy of the same callback, so we only need to
% retain the first antiunificand's copy.
is_first_antiunificand(V, anti_unify(V1, _, _)) :- V == V1.

same_functor(A, B) :-
    functor(A, Name, Arity),
    functor(B, Name, Arity).
