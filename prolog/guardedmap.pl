:- module(guardedmap, [guardedmap/5]).

:- autoload(library(apply), [maplist/3, maplist/2, maplist/4]).
:- autoload(library(rbtrees), [rb_empty/1, rb_insert_new/4]).

:- meta_predicate guardedmap(3, 3, +, +, +).

guardedmap(Guard, Goal, A, B, C) :-
    writeln(guardedmap(Guard, Goal, A, B, C)),
    rb_empty(Seen),
    guardedmap_(Guard, Goal, Seen, A, B, C).

guardedmap_(Guard, Goal, Seen, A, B, C) :-
    call(Guard, A, B, C)                 -> call(Goal, A, B, C) ;
    first_visit(f(A, B, C), Seen, Seen1) -> mapargs(guardedmap_(Guard, Goal, Seen1), A, B, C) ;
    true.

first_visit(Term, Seen0, Seen) :-
    rb_insert_new(Seen0, Term, 1, Seen).

mapargs(Goal, A, B, C) :-
    name_arity_args_term(Name, Arity, As, A),
    name_arity_args_term(Name, Arity, Bs, B),
    name_arity_args_term(Name, Arity, Cs, C),
    maplist(Goal, As, Bs, Cs).

name_arity_args_term(Name, Arity, Args, Term) :-
    functor(Term, Name, Arity),
    Term =.. [_|Args].
