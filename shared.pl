
:- use_module(library(clpfd)).

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
