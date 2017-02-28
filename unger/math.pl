
:- use_module(library(clpfd)).

%% Grammar from page 105
% Expr_s -> Expr + Term | Term
% Term   -> Term x Factor | Factor
% Factor -> ( Expr ) | i

%% [
%%     expr   : [[expr, '+', term], [term]],
%%     term   : [[term, 'x', factor], [factor]],
%%     factor : [['(', expr, ')'], ['i']]
%% ]

parse(Input, Tree) :-
    string_chars(Input, Chars),
    delete(Chars, ' ', CharsNoSpaces),
    parse_expr(CharsNoSpaces, Tree).

parse_expr(Input, expr(Left, '+', Right)) :-
    partition(3, Input, Output),
    Output = [Expr, ['+'], Term],
    parse_expr(Expr, Left),
    parse_term(Term, Right).
parse_expr(Input, expr(Tree)) :-
    partition(1, Input, Output),
    Output = [Term],
    parse_term(Term, Tree).

parse_term(Input, term(Left, 'x', Right)) :-
    partition(3, Input, Output),
    Output = [Term, ['x'], Factor],
    parse_term(Term, Left),
    parse_factor(Factor, Right).
parse_term(Input, term(Tree)) :-
    partition(1, Input, Output),
    Output = [Factor],
    parse_factor(Factor, Tree).

parse_factor(Input, factor('(', Tree, ')')) :-
    partition(3, Input, Output),
    Output = [['('], Expr, [')']],
    parse_expr(Expr, Tree).
parse_factor(Input, factor('i')) :-
    partition(1, Input, Output),
    Output = [['i']].

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

:- begin_tests(partition).

test("partition fails when given []",
     fail) :-
    partition(_, [], _).

test("partition output is [input] on one partition",
     true(Output == [[1]])) :-
    partition(1, [1], Output).

test("partition fails when partitions > input length", fail) :-
    partition(2, [_], _).

test("partition : length(Input, Count) => length(Output, Count)",
     [nondet,
      true(length(Output, Count))]) :-
    Input = [1,2,3,4,5],
    length(Input, Count),
    partition(Count, Input, Output),
    assertion(flatten(Output, Input)).

test("partition produces multiple solutions",
     all(Output == [ [[1], [2, 3, 4]],
		     [[1, 2], [3, 4]],
		     [[1, 2, 3], [4]] ])) :-
    partition(2, [1, 2, 3, 4], Output).

:- end_tests(partition).
