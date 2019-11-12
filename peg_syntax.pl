:- module(peg_syntax,
         [op(500,xf, '?'),
          op(500,fx, '!'),
          op(500,fx, '&'),
          op(500,xf, '*'),
          op(500,xf, '+'),
          op(1100,xfy, '/'),
          op(1200, xfx, '<--'),
          op(1200, xfx, '<=='),
          term_expansion/2]).

expand_match_clause(X, Clause, (match(Orig, Clause),{X=token(Clause, Orig)})) :-
    atom(Clause),
    !.

expand_match_clause(_, Orig, _) :-
    throw(bad_clause(Orig)).

expand_match_branch(X, (Clause;Clauses), (Clause2;Clauses2)) :-
    !,
    expand_match_clause(X, Clause, Clause2),
    expand_match_branch(X, Clauses, Clauses2).

expand_match_branch(X, Clause, Clause2) :-
    expand_match_clause(X, Clause, Clause2).

term_expansion('<--'(Head, Clauses), '-->'(Head, Clauses2)) :-
    dcg(Clauses, Clauses2).

term_expansion('<=='(Head, Clauses), Result) :-
    Result_Unexpanded = '<--'(Head2, Clauses2_Unexpanded),
    !,
    Head2 =.. [Head, X],
    expand_match_branch(X, Clauses, Clauses2_Unexpanded),
    expand_term(Result_Unexpanded, Result).

:- use_module(peg).
