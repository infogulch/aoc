:- module(day2, [
    ws//0,
    numeric_chars//1,
    num//1,
    list_max/3
]).

:- use_module(library(dcgs)).
:- use_module(library(lists)).

:- use_module(input).
:- use_module(day1).

list_max([],[],[]).
list_max([A|As],[B|Bs],[M|Ms]) :- M is max(A,B),list_max(As,Bs,Ms).

ws --> ""|" ",ws.

numeric_chars([C|Cs]) --> [C],{char_type(C,numeric)},numeric(Cs).
numeric_chars("") --> "".
num(N) --> numeric_chars(Cs),{Cs=[_|_],number_chars(N,Cs)}.

game(Id,Rs) --> "Game ",num(Id),":",rounds(Rs).

rounds([C]) --> ws,round(C).
rounds([C|Cs]) --> ws,round(C),ws,";",rounds(Cs).

round(C) --> color(C).
round(C) --> color(C0),",",round(C1),{list_max(C0,C1,C)}.

color([R,0,0]) --> ws,num(R),ws,"red".
color([0,G,0]) --> ws,num(G),ws,"green".
color([0,0,B]) --> ws,num(B),ws,"blue".

games(Gs) :-
    input(2,Input),
    phrase(lines(Lines),Input),
    maplist(line_game,Lines,Gs).

line_game(Line,Id-Rounds) :-
    once(phrase(game(Id,Rounds),Line)).

part1(Sum) :-
    games(Gs),
    maplist(game_max_id,Gs,Ids),
    sum_list(Ids,Sum).

game_max_id(Id-RoundColors,Id0) :-
    transpose(RoundColors,ColorRounds),
    maplist(list_max, ColorRounds, [MaxR,MaxG,MaxB]),
    (MaxR =< 12,MaxG =< 13,MaxB =< 14 -> Id0 = Id; Id0 = 0).


part2(Sum) :-
    games(Gs),
    maplist(game_max_product,Gs,Products),
    sum_list(Products,Sum).

game_max_product(_-Rounds,Product) :-
    transpose(RoundColors,ColorRounds),
    maplist(list_max, ColorRounds, [MaxR,MaxG,MaxB]),
    Product is MaxR*MaxG*MaxB.

run_infogulch :- part1(3035),part2(66027).
:- initialization(run_infogulch).
