:- module(day4,[]).

:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(lambda)).
:- use_module(library(charsio)).
:- use_module(library(ordsets)).
:- use_module(library(reif)).
:- use_module(library(dif)).

:- use_module(library(debug)).

:- use_module(input).
:- use_module(day1).
:- use_module(day2).

nums([]) --> ws.
nums([N|Ns]) --> " ",ws,num(N),nums(Ns).

card(Id,Ws,Hs) --> "Card",ws,num(Id),":",nums(Ws),"|",nums(Hs).

card_score(Line,MatchCount) :-
    phrase(card(_Id,Wins0,Haves0),Line),
    maplist(list_to_ord_set,[Wins0,Haves0],[Wins,Haves]),
    ord_intersection(Wins,Haves,Matches),
    length(Matches,MatchCount).

cards(Input,MatchCounts) :-
    phrase(lines(Lines),Input),
    maplist(card_score,Lines,MatchCounts).


part1(A) :-
    input(4,Input),
    cards(Input,MatchCounts),
    maplist(\N^P^if_(N=0,P=0,P is 2^(N-1)),MatchCounts,Points),
    sum_list(Points,Total),
    A = Total.


addxntimes(_,0,Ls,Ls).
addxntimes(X,N,[A|As],[B|Bs]) :- dif(N,0), B is A+X, N0 is N-1, addxntimes(X,N0,As,Bs).

% check:
% r(0,30,[4,2,2,1,0,0],[1,1,1,1,1,1])

r(T,T,[],[]).
r(Total0,Total,[M|Matches],[C|Cards]) :- Total1 is Total0+C, addxntimes(C,M,Cards,Cards0), r(Total1,Total,Matches,Cards0).

part2(A) :-
    input(4,Input),
    cards(Input,MatchCounts),
    maplist(\_^X^(X=1),MatchCounts,CardCounts),
    r(0,Total,MatchCounts,CardCounts),
    A = Total.
