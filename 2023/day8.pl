:- module(day8,[part1/1,part2/1]).

:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(pio)).

:- use_module(library(debug)).

:- use_module(input).
:- use_module(day2).
:- use_module(day4).
:- use_module(day5).

lrs([C|Cs]) --> [C],{C='L';C='R'},lrs(Cs).
lrs([]) --> "".

id(Id) --> {length(Id,3)},chars_type(Id,alpha).

net([H-(L-R)|Ns]) --> id(H)," = (",id(L),", ",id(R),")\n",net(Ns).
net([]) --> [].

document(Dirs,Net) --> lrs(Dirs),"\n\n",net(Net).

follow(_,_,_,Key,Key,0).
follow(A,Dirs0,[],Key,TargetKey,N) :- follow(A,Dirs0,Dirs0,Key,TargetKey,N).
follow(A,Dirs0,[D|Dirs],Key,TargetKey,N) :- get_assoc(Key,A,L-R),(D='L',follow(A,Dirs0,Dirs,L,TargetKey,N0);D='R',follow(A,Dirs0,Dirs,R,TargetKey,N0)),N is N0+1.

part1(Steps) :-
    input(8,Input),
    phrase(document(Dirs,Net), Input),
    list_to_assoc(Net,Assoc),
    once(follow(Assoc,Dirs,Dirs,"AAA","ZZZ",Steps)).

part1s(Steps) :-
    phrase_from_file(seq(Input),"inputs/8sample.txt"),
    phrase(document(Dirs,Net), Input),
    list_to_assoc(Net,Assoc),
    once(follow(Assoc,Dirs,Dirs,"AAA","ZZZ",Steps)).

part2(A) :-
    input(8,Input),
    A = Input.
