:- module(day8,[part1/1,part2/1]).

:- use_module(library(assoc)).
:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(reif)).
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
follow(A,Dirs0,[D|Dirs],Key,TargetKey,N) :- next(A,D,Key,NKey),follow(A,Dirs0,Dirs,NKey,TargetKey,N0),N is N0+1.

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

ends_with(L, [_,_,E], T) :- =(L, E, T).

next(A,D,Key,NKey) :- get_assoc(Key,A,L-R),if_(D='L',NKey=L,NKey=R).

next_all(A,D,Keys,NKeys) :- maplist(next(A,D),Keys,NKeys).

tall(If_1, Ls0, T) :- tfilter(If_1,Ls0,Ls1),length(Ls0,N0),length(Ls1,N1),=(N0,N1,T).

follow_all(A,Dirs0,[],Keys,N0,N) :- follow_all(A,Dirs0,Dirs0,Keys,N0,N).
follow_all(A,Dirs0,[D|Dirs],Keys,N0,N) :- if_(tall(ends_with('Z'),Keys), N=N0, (next_all(A,D,Keys,NKeys),N1 is N0+1,follow_all(A,Dirs0,Dirs,NKeys,N1,N))).

/* theoretically a correct brute force solution; but it appears that the actual solution may require trillions of iterations, thus needs a more analytical approach */
part2slow(N) :-
    input(8,Input),
    phrase(document(Dirs,Net), Input),
    list_to_assoc(Net,Assoc),
    assoc_to_keys(Assoc,Keys),
    tfilter(ends_with('A'), Keys, AKeys),
    follow_all(Assoc,Dirs,Dirs,AKeys,0,N).

part2(0).

/*
I guess you can't use tfilter with lambda?
*/
