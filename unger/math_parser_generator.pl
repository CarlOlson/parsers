
:- ['../shared.pl'].

%% Grammar from page 105
% Expr_s -> Expr + Term | Term
% Term   -> Term x Factor | Factor
% Factor -> ( Expr ) | i

start_rule(expr).

rule(expr, [expr, '+', term]).
rule(expr, [term]).
rule(term, [term, 'x', factor]).
rule(term, [factor]).
rule(factor, ['(', expr, ')']).
rule(factor, ['i']).

parse(Input, Tree) :-
    string_chars(Input, Chars),
    delete(Chars, ' ', CharsNoSpaces),
    start_rule(Rule),
    try_parse(Rule, CharsNoSpaces, Tree).

try_parse(Rule, Input, Tree) :-
    rule(Rule, Rhs),
    length(Rhs, Length),
    partition(Length, Input, Output),
    match(Rhs, Output, Tree0),
    Tree =.. [Rule | Tree0].

match([], [], []).
match([Char | Xs], [[Char] | Ys], [Char|Tail]) :-
    is_char(Char),
    match(Xs, Ys, Tail).
match([Rule | Xs], [Y | Ys], [Head|Tail]) :-
    try_parse(Rule, Y, Head),
    match(Xs, Ys, Tail).
