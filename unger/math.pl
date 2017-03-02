
:- ['../shared.pl'].

%% Grammar from page 105
% Expr_s -> Expr + Term | Term
% Term   -> Term x Factor | Factor
% Factor -> ( Expr ) | i

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
