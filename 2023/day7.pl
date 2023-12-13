:- module(day7,[part1/1,part2/1]).

:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(lambda)).
:- use_module(library(charsio)).
:- use_module(library(debug)).

:- use_module(input).
:- use_module(day2).
:- use_module(day4).
:- use_module(day5).

game([H-B|HBs]) --> seq(H),{length(H,5)}," ",num(B),"\n",game(HBs).
game([]) --> "".

part1(A) :-
    input(7,Input),
    phrase(game(G), Input),
    A = G.

part1s(A) :-
    phrase_from_file(seq(Input),"inputs/7sample.txt"),
    phrase(game(G), Input),
    A = G.

part2(A) :-
    input(7,Input),
    A = Input.


