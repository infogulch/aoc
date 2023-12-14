:- module(day11,[part1/1,part2/1,choose/3,nats/2]).

:- use_module(library(lists)).
:- use_module(library(dcgs)).
:- use_module(library(ordsets)).
:- use_module(library(lambda)).
:- use_module(library(dif)).
:- use_module(library(between)).
:- use_module(library(pio)).

:- use_module(library(debug)).

:- use_module(input).
:- use_module(util).

cols(Gs,R,C) --> ".",{C1 is C+1},cols(Gs,R,C1).
cols([R-C|Gs],R,C) --> "#",{C1 is C+1},cols(Gs,R,C1).
cols([],_,_) --> "".

rows(Gs,R) --> cols(Rgs,R,0),"\n",{R1 is R+1},rows(Gs1,R1),{append(Rgs,Gs1,Gs)}.
rows([],_) --> "".

image(Gs) --> rows(Gs,0).

split(Gs,Rs,Cs) :-
    maplist(\G^A^B^(G=A-B),Gs,Rs1,Cs1),
    list_to_ord_set(Rs1,Rs),
    list_to_ord_set(Cs1,Cs).

nats(N,Ls) :- nats(0,N,Ls).
nats(N,N,[N]).
nats(C,N,[C|Ls]) :- dif(C,N),C1 is C+1,nats(C1,N,Ls).

missing(Gs,MRs,MCs) :-
    split(Gs,Rows,Cols),
    list_max(Rows,MaxRows),nats(MaxRows,AllRows),ord_subtract(AllRows,Rows,MRs),
    list_max(Cols,MaxCols),nats(MaxCols,AllCols),ord_subtract(AllCols,Cols,MCs).

choose(_,[],[]).
choose(1,[L|Ls],Ls1) :- transpose([[L|Ls]],Ls1).
choose(N,[L|Ls],Cs) :-
    dif(N,1),
    N1 is N-1,
    choose(N1,Ls,Cs1),
    maplist(append([L]),Cs1,CsA),
    choose(N,Ls,CsB),
    append(CsA,CsB,Cs).

count_between(A,A,_,0).
count_between(X,Y,Ls,N) :- sort([X,Y],[X1,Y1]),count_between_(X1,Y1,Ls,N).

count_between_(_,_,[],0).
count_between_(X,Y,[L|Ls],N) :- (between(X,Y,L) -> N0=1; N0=0),count_between_(X,Y,Ls,N1),N is N0+N1.

dist(MRs,MCs,[R1-C1,R2-C2],Dist) :- count_between(R1,R2,MRs,Rn), count_between(C1,C2,MCs,Cn), Dist is abs(R1-R2)+abs(C1-C2)+Rn+Cn.

part1(Total) :-
    input(11,Input),
    phrase(image(Gs),Input),
    list_to_ord_set(Gs,Galaxies),
    missing(Galaxies,MissingRows,MissingCols),
    choose(2,Galaxies,Pairs),
    maplist(dist(MissingRows,MissingCols),Pairs,Dists),
    sum_list(Dists,Total).

dist2(MRs,MCs,[R1-C1,R2-C2],Dist) :- count_between(R1,R2,MRs,Rn), count_between(C1,C2,MCs,Cn), Dist is abs(R1-R2)+abs(C1-C2)+(Rn+Cn)*(1000000-1).

part2(Total) :-
    input(11,Input),
    phrase(image(Gs),Input),
    list_to_ord_set(Gs,Galaxies),
    missing(Galaxies,MissingRows,MissingCols),
    choose(2,Galaxies,Pairs),
    maplist(dist2(MissingRows,MissingCols),Pairs,Dists),
    sum_list(Dists,Total).

dist2s(MRs,MCs,[R1-C1,R2-C2],Dist) :- count_between(R1,R2,MRs,Rn), count_between(C1,C2,MCs,Cn), $ Dist is abs(R1-R2)+abs(C1-C2)+(Rn+Cn)*(100-1).

part2s(Total) :-
    $ phrase_from_file(seq(Input),"inputs/11sample.txt"),
    phrase(image(Gs),Input),
    list_to_ord_set(Gs,Galaxies),
    $ missing(Galaxies,MissingRows,MissingCols),
    choose(2,Galaxies,Pairs),
    maplist(dist2s(MissingRows,MissingCols),Pairs,Dists),
    sum_list(Dists,Total).

% 790195502522 too high
