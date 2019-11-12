:- module(peg, [dcg/2, peg_phrase/2]).
:- use_module(dcg_util).
:- reexport([dcg_util]).
:- use_module(peg_syntax).

dcg(X/Y, (X2*->[];Y2)) :-
    !,
    dcg(X, X2),
    dcg(Y, Y2).

dcg(X;Y, (X2;Y2)) :-
    !,
    dcg(X, X2),
    dcg(Y, Y2).

dcg((X,Y), (X2,Y2)) :-
    !,
    dcg(X, X2),
    dcg(Y, Y2).
dcg(X?, optional(X2)) :-
    !,
    dcg(X, X2).

dcg('&'(X), peek(X2)) :-
    !,
    dcg(X, X2).

dcg('!'(X), (\+ X2)) :-
    !,
    dcg(X, X2).

dcg('+'(X), plus(X2)) :-
    !,
    dcg(X, X2).

dcg('*'(X), star(X2)) :-
    !,
    dcg(X, X2).

dcg(any(X), X2) :-
    !,
    dcg(X, X_Transformed),
    (   X_Transformed = (X->[])
    ->  X2 = X
    ;   X2 = X_Transformed).

dcg(X, (X->[])).


peg_phrase(Phrase, List) :-
    dcg(Phrase, Dcg_Phrase),
    phrase(Dcg_Phrase, List).

peg_phrase(Phrase, List, Remainder) :-
    dcg(Phrase, Dcg_Phrase),
    phrase(Dcg_Phrase, List, Remainder).
