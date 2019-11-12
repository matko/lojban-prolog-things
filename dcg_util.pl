:- module(dcg_util, [match/4, peek/3, star/3, plus/3, optional/3]).

:-meta_predicate(match(-, //, ?, ?)).
match(M, X, A, B) :-
    phrase(X, A, B),
    once(append(M, B, A)). % todo: is there a better way to get difference list?

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

unpack_tokens_processor([token(T,_)|Ts], Ts, T).

unpack_tokens(Ts, Elts) :-
    lazy_list(unpack_tokens_processor, Ts, Elts).
