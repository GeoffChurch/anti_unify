:- module(guardedmap, [guardedmap/5]).

:- autoload(library(apply), [foldl/6]).
:- autoload(library(rbtrees), [rb_empty/1, rb_insert_new/4, rb_lookup/3]).

:- meta_predicate guardedmap(3, 3, +, +, +).

guardedmap(Guard, Goal, A, B, C) :-
    rb_empty(Seen0),
    guardedmap_(Guard, Goal, A, B, C, Seen0, _).

guardedmap_(Guard, Goal, A, B, C) -->
    insert_new(k(A, B), C)
    ->  first_visit(Guard, Goal, A, B, C)
    ;   return_visit(A, B, C).

insert_new(Key, Value, Seen0, Seen) :-
    rb_insert_new(Seen0, Key, Value, Seen).

first_visit(Guard, Goal, A, B, C) -->
    { call(Guard, A, B, C) }
    ->  { call(Goal, A, B, C) }
    ;   foldargs(guardedmap_(Guard, Goal), A, B, C).

return_visit(A, B, C, Seen, Seen) :-
    rb_lookup(k(A, B), C, Seen).

foldargs(Goal, A, B, C) -->
    {
	name_arity_args_term(Name, Arity, As, A),
	name_arity_args_term(Name, Arity, Bs, B),
	name_arity_args_term(Name, Arity, Cs, C)
    },
    foldl(Goal, As, Bs, Cs).

name_arity_args_term(Name, Arity, Args, Term) :-
    functor(Term, Name, Arity),
    Term =.. [_|Args].
