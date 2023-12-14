:- module(day10,[part1/1,part2/1]).

:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(reif)).
:- use_module(library(pio)).
:- use_module(library(assoc)).
:- use_module(library(dif)).

:- use_module(library(debug)).

:- use_module(input).
:- use_module(util).

% Px=previous x, Nx=next x
conn(R,C,(PR-C)-(NR-C)) --> "|",{PR is R-1,NR is R+1}.
conn(R,C,(R-PC)-(R-NC)) --> "-",{PC is C-1,NC is C+1}.
conn(R,C,(PR-C)-(R-NC)) --> "L",{PR is R-1,NC is C+1}.
conn(R,C,(PR-C)-(R-PC)) --> "J",{PR is R-1,PC is C-1}.
conn(R,C,(R-PC)-(NR-C)) --> "7",{NR is R+1,PC is C-1}.
conn(R,C,(NR-C)-(R-NC)) --> "F",{NR is R+1,NC is C+1}.
conn(_R,_C,'S') --> "S".

row(KV,R,C) --> ".",{C1 is C+1},row(KV,R,C1).
row([(R-C)-V|KV],R,C) --> conn(R,C,V),({C1 is C+1},row(KV,R,C1)).
row([],_,_) --> "".

rows(KV,N) --> row(KV0,N,0),"\n",({N1 is N+1},rows(KV1,N1);"",{KV1=[]}),{append(KV0,KV1,KV)}.

map(KV) --> rows(KV,0).

path_length(_,Start,_,Start,N,N).
path_length(A,Start,Prev,K,N0,N) :- dif(Start,K),get_assoc(K,A,K1-K2),(K1==Prev,Next=K2;K2==Prev,Next=K1),N1 is N0+1,path_length(A,Start,K,Next,N1,N).

connected(A,R-C,M) :- get_assoc(R-C,A,K1-K2),(K1=M;K2=M).

adj(A,R-C,Next1-Next2) :- member(L,"|-LJ7F"),phrase(conn(R,C,Next1-Next2),[L]),connected(A,Next1,R-C),connected(A,Next2,R-C),_A=found(L).

part1(A) :-
    input(10,Input),
    phrase(map(KV),Input),
    member(K-'S', KV),
    list_to_assoc(KV,Assoc),
    adj(Assoc,K,Next-_),
    path_length(Assoc,K,K,Next,0,N),
    A is round(N/2).

get_path(_,Start,_,Start,P,P).
get_path(A,Start,Prev,K,P0,P) :- dif(Start,K),get_assoc(K,A,K1-K2),(K1==Prev,Next=K2;K2==Prev,Next=K1),get_path(A,Start,K,Next,[K|P0],P).

part2(A) :-
    input(10,Input),
    phrase(map(KV),Input),
    member(K-'S', KV),
    list_to_assoc(KV,Assoc),
    adj(Assoc,K,Next-_),
    get_path(Assoc,K,K,Next,[],P),
    A = P.
