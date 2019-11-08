:- module(dcg_util, [match/4, peek/3, star/3, plus/3, optional/3, star_comma/2]).

:-meta_predicate(match(//, ?, ?, ?)).
match(X, M, A, B) :-
    phrase(X, A, B),
    once(append(M, B, A)).

:-meta_predicate(peek(//, ?, ?)).
peek(X,A,A) :-
    once(phrase(X, A, _)).

:-meta_predicate(star(//, ?, ?)).
star(X) -->
    X,
    !,
    star(X).
star(_X) --> [].

:-meta_predicate(plus(//, ?, ?)).
plus(X) -->
    X,
    star(X).

:-meta_predicate(optional(//, ?, ?)).
optional(X,A,B) :-
    (   phrase(X, A, B)
    ->  true
    ;   A = B).

star_comma(A, B) :-
    var(A), !, B = A.
star_comma([44|L], B) :-
    !,
    star_comma(L, B).
star_comma(B,B).
