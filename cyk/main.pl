
:- ['../shared.pl'].

start_rule(number).

rule(number, [integer]).
rule(number, [real]).
rule(integer, [digit]).
rule(integer, [digit, integer]).
rule(real, [integer, fraction]).
rule(real, [integer, fraction, scale]).
rule(fraction, ['.', integer]).
rule(scale, ['e', sign, integer]).
%% rule(scale, [empty]).
rule(digit, [D]) :-
    string_chars("0123456789", Digits),
    member(D, Digits).
rule(sign, ['+']).
rule(sign, ['-']).

parse(Input, Output) :-
    string_chars(Input, Chars),
    make_matrix(Chars, Matrix),
    parseM(Matrix, Output),
    [[Rules|_]|_] = Output,
    start_rule(StartRule),
    member(StartRule, Rules).

equivalent_term(Term, Term).
equivalent_term(Term, NextTerm) :-
    rule(EqTerm, [Term]),
    equivalent_term(EqTerm, NextTerm).

make_possible(Token, Terms) :-
    setof(Term, equivalent_term(Token, Term), Terms).

make_matrix([], []).
make_matrix([T|Ts], [[C]|Cs]) :-
    make_possible(T, C),
    make_matrix(Ts, Cs).

dimensions([Col|Cols], RowLength, ColLength) :-
    length([Col|Cols], RowLength),
    length(Col, ColLength).

is_done(Matrix) :-
    dimensions(Matrix, RowLength, ColLength),
    ColLength >= RowLength.

parseM(Matrix, Out) :-
    \+ is_done(Matrix),
    dimensions(Matrix, _, ColLength),
    N is ColLength + 1,
    parseNext(N, Matrix, Next),
    parseM(Next, Out).
parseM(Matrix, Matrix) :-
    is_done(Matrix).

parseNext(N, [Col|Cols], Out) :-
    dimensions([Col|Cols], TermsLeft, _),
    (TermsLeft >= N ->
	 possible_terms(N, [Col|Cols], NewRow),
	 Out = [[NewRow|Col]|OutTail],
	 parseNext(N, Cols, OutTail);
     Out = [Col|Cols]).

members_size([X|Xs], X, Size) :-
    length([X|Xs], Size).
members_size([_|Xs], X, Size) :-
    members_size(Xs, X, Size).

possible_terms(N, Matrix, Possible) :-
    setof(Term, possible_term(N, Matrix, [], Term), Possible);
    Possible = [].

possible_term(0, _, InRev, Out) :-
    reverse(In, InRev),
    rule(Term, In),
    equivalent_term(Term, Out).
possible_term(N, [_|Cols], Terms, Out) :-
    N > 0,
    possible_term(N, Cols, Terms, Out).
possible_term(N, [Col|Cols], Terms, Out) :-
    N > 0,
    members_size(Col, Row, Size),
    member(Term, Row),
    N1 is N - Size,
    possible_term(N1, Cols, [Term|Terms], Out).

:- begin_tests(cyk).

test(is_done) :-
    \+ is_done([[1], [1]]),
    is_done([[1, 2, 3], [1, 2, 3]]).

test(members_size, nondet) :-
    members_size([1, 2], 2, 1),
    members_size([1, 2, 3], 1, 3).

:- end_tests(cyk).
