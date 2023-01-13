:- module(guardedmap, [guardedmap/3]).

:- use_module(library(clpfd), [transpose/2]).

:- meta_predicate guardedmap(:, :, +).

% TODO goal expansion

guardedmap(Guard, Goal, Terms) :-
    rb_empty(Seen),
    guardedmap_(Guard, Goal, Seen, Terms).

guardedmap_(Guard, Goal, Seen, Terms) :-
    apply(Guard, Terms)              ->  apply(Goal, Terms) ;
    first_visit(Terms, Seen, Seen1)  ->  mapargs(guardedmap_(Guard, Goal, Seen1), Terms) ;
    true.


%%% UTILS %%%

first_visit(Term, Seen0, Seen) :-
    rb_insert_new(Seen0, Term, 1, Seen).

mapargs(Goal, Terms) :-
    maplist(name_arity_args_term(_,_), As0, Terms),
    transpose(As0, As),
    maplist(Goal, As).

name_arity_args_term(Name, Arity, Args, Term) :-
    functor(Term, Name, Arity),
    Term =.. [_|Args].
