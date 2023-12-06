:- module(day1,[
    peek//1,
    eof//0,
    lines//1
]).

:- use_module(library(dcgs)).
:- use_module(library(dif)).
:- use_module(library(lists)).
:- use_module(library(charsio)).
:- use_module(library(lambda)).

:- use_module(input).

peek(T), [T] --> [T].

eof_([], []).
eof --> call(eof_), !.

lines([]) --> eof.
lines([L|Ls]) --> line(L),lines(Ls).

line([]) --> "\n".
line([C|Cs]) --> [C],{dif('\n',C)},line(Cs).


part1(Result) :-
    input(1,Input),
    phrase(lines(Lines), Input),
    maplist(line_number, Lines, Numbers),
    sum_list(Numbers,Result).

line_number(Line,Number) :-
    chars_digits(Line, Digits),
    phrase((peek(First),...,[Last]), Digits),
    number_chars(Number,[First,Last]).

chars_digits([],[]).
chars_digits([C|Chars],Digits) :- (char_type(C,numeric) -> chars_digits(Chars,Digits0),Digits = [C|Digits0]; chars_digits(Chars,Digits)).


part2(Result) :-
    input(1,Input),
    phrase(lines(Lines), Input),
    maplist(line_number2, Lines, Numbers),
    sum_list(Numbers,Result).

line_number2(Line,Number) :-
    once(phrase(numeric_string(Digits),Line)),
    phrase((peek(First),...,[Last]), Digits),
    Number is First*10+Last.

spelled_number(1),"e" --> "one".
spelled_number(2),"o" --> "two".
spelled_number(3),"e" --> "three".
spelled_number(4),"r" --> "four".
spelled_number(5),"e" --> "five".
spelled_number(6),"x" --> "six".
spelled_number(7),"n" --> "seven".
spelled_number(8),"t" --> "eight".
spelled_number(9),"e" --> "nine".
spelled_number(0),"o" --> "zero".

numeric_string([]) --> eof.
numeric_string([N|R]) --> [C], {char_type(C,numeric),number_chars(N,[C])},numeric_string(R).
numeric_string([N|R]) --> spelled_number(N),numeric_string(R).
numeric_string(R) --> [_],numeric_string(R).


run_infogulch :- part1(54632),part2(54019).
% :- initialization(run_infogulch).
