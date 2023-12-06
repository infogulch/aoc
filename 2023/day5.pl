:- module(day5,[part1/1]).

:- use_module(library(dcgs)).
:- use_module(library(dif)).
:- use_module(library(lists)).
:- use_module(library(charsio)).
:- use_module(library(lambda)).

:- use_module(library(debug)).

:- use_module(input).

:- use_module(day1).
:- use_module(day2).
:- use_module(day4).

chars_type([C|Cs],Type) --> [C],{char_type(C,Type)},chars_type(Cs,Type).
chars_type("",_) --> "".

intervals(Sa,Ea,Sb,Eb,Offset,Count) :- Sa is Sb+Offset,Ea is Eb+Offset,Count is Ea-Sa.

almanac(Seeds,Maps) --> "seeds:",ws,nums(Seeds),"\n",maps(Maps).

maps([M|Maps]) --> "\n",chars_type(_A,alpha),"-to-",chars_type(_B,alpha)," map:\n",ranges(M),!,maps(Maps).
maps([]) --> eof.

ranges([[Dst,Src,Len]|M]) --> nums([Dst,Src,Len]),!,"\n",ranges(M).
ranges([]) --> "".

part1(A) :-
    input(5,Input),
    phrase(almanac(Seeds,Maps),Input),
    A = seeds_maps(Seeds,Maps).
