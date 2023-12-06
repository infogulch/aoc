:- module(day5,[part1/1]).

:- use_module(library(dcgs)).
:- use_module(library(dif)).
:- use_module(library(lists)).
:- use_module(library(charsio)).
:- use_module(library(lambda)).
:- use_module(library(reif)).
:- use_module(library(between)).

:- use_module(library(debug)).
:- use_module(library(format)).

:- use_module(input).

:- use_module(day1).
:- use_module(day2).
:- use_module(day4).

chars_type([C|Cs],Type) --> [C],{char_type(C,Type)},chars_type(Cs,Type).
chars_type("",_) --> "".

intervals(Sa,Ea,Sb,Eb,Offset,Count) :- Sa is Sb+Offset,Ea is Eb+Offset,Count is Ea-Sa.

% parse the almanac
almanac(Seeds,Maps) --> "seeds:",ws,nums(Seeds),"\n",maps(Maps).

maps([M|Maps]) --> "\n",chars_type(_A,alpha),"-to-",chars_type(_B,alpha)," map:\n",ranges(M),!,maps(Maps).
maps([]) --> eof.

ranges([[Dst,Src,Len]|M]) --> nums([Dst,Src,Len]),!,"\n",ranges(M).
ranges([]) --> "".

% map an id
map_id(Id0,Id1,[[Dst,Src,Len]|Map]) :-
    SrcMax is Src+Len,
    between(Src,SrcMax,Id0) -> Id1 is Id0+(Dst-Src) ; map_id(Id0,Id1,Map).
map_id(Id,Id,[]).

maps_ids(Ids0,Ids1,Maps) :- maplist(\Id0^Id1^foldl(\M^IdN^IdM^map_id(IdN,IdM,M), Maps, Id0, Id1), Ids0, Ids1).

part1(Min) :-
    input(5,Input),
    phrase(almanac(Seeds0,Maps),Input),
    maps_ids(Seeds0, [L|LocationIds], Maps),
    foldl(\A^B^Min^(Min is min(A,B)), LocationIds, L, Min).
