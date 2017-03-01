
:- use_module(library(clpfd)).

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

take(0, Rest, []:Rest).
take(N0, [A | As], [A | Bs]:Rest) :-
    N #= N0 - 1,
    take(N, As, Bs:Rest).

partition(N0, Input, [Group | Groups]) :-
    take(_, Input, Group:Rest),
    Group \= [],
    N #= N0 - 1,
    partition(N, Rest, Groups).
partition(1, Input, [Input]) :-
    is_list(Input),
    Input \== [].

is_char(Char) :-
    atom(Char),
    catch(char_code(Char, _), Failed, true),
    var(Failed).
