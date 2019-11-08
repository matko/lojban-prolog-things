:- module(peg_syntax,
         [op(500,xf, '?'),
          op(500,fx, '!'),
          op(500,fx, '&'),
          op(500,xf, '*'),
          op(500,xf, '+'),
          op(1200, xfx, '<--'),
          term_expansion/2]).
:- use_module(peg).

term_expansion('<--'(Head, Clauses), '-->'(Head, Clauses2)) :-
    dcg(Clauses, Clauses2).
