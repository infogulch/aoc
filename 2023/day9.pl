:- module(day9,[part1/1,part2/1]).

:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(reif)).
:- use_module(library(pio)).

:- use_module(library(debug)).

:- use_module(input).
:- use_module(util).

histories([H|Hs]) --> nnums(H),"\n",histories(Hs).
histories([]) --> "".

pairwise_diff([A,B|Ls],[D|Ds]) :- D is B-A,pairwise_diff([B|Ls],Ds).
pairwise_diff([_],[]).

last([X],X).
last([_,X|Xs],Y) :- last([X|Xs],Y).

next([],0).
next(As,N) :- pairwise_diff(As,Bs),last(As,A),next(Bs,B),N is A+B.

part1(Sum) :-
    input(9,Input),
    phrase(histories(Hs),Input),
    maplist(next,Hs,Nexts),
    sum_list(Nexts,Sum).

part1s(Sum) :-
    phrase_from_file(seq(Input),"inputs/9sample.txt"),
    phrase(histories(Hs),Input),
    maplist(next,Hs,Nexts),
    sum_list(Nexts,Sum).

part2(2).
