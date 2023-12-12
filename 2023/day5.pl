:- module(day5,[part1/1,part2/1]).

:- use_module(library(dcgs)).
:- use_module(library(dif)).
:- use_module(library(lists)).
:- use_module(library(charsio)).
:- use_module(library(lambda)).
:- use_module(library(reif)).
:- use_module(library(between)).

:- use_module(library(debug)).
:- use_module(library(format)).
:- use_module(library(pio)).

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

pairs([A-B|Ps]) --> [A,B],pairs(Ps).
pairs([]) --> [].

idrange_map(R, [], [R]).
idrange_map(IdStart-IdEnd, [_-SrcStart-SrcEnd|Ranges], Mapped) :- (IdEnd < SrcStart; SrcEnd < IdStart), idrange_map(IdStart-IdEnd, Ranges, Mapped).
idrange_map(IdStart-IdEnd, [Delta-SrcStart-SrcEnd|_], [Start-End]) :- SrcStart =< IdStart, IdEnd =< SrcEnd, (Start is IdStart+Delta), (End is IdEnd+Delta).
idrange_map(IdStart-IdEnd, [Delta-SrcStart-SrcEnd|Ranges], [Start-End|Mapped]) :- IdStart < SrcStart, IdEnd =< SrcEnd, PSrcStart is SrcStart-1, idrange_map(IdStart-PSrcStart,Ranges,Mapped), Start is SrcStart+Delta, End is IdEnd+Delta.
idrange_map(IdStart-IdEnd, [Delta-SrcStart-SrcEnd|Ranges], [Start-End|Mapped]) :- SrcStart =< IdStart, SrcEnd < IdEnd, NSrcEnd is SrcEnd+1, idrange_map(NSrcEnd-IdEnd,Ranges,Mapped), (Start is IdStart+Delta), (End is SrcEnd+Delta).
idrange_map(IdStart-IdEnd, [Delta-SrcStart-SrcEnd|Ranges], [Start-End|Mapped]) :- IdStart < SrcStart, SrcEnd < IdEnd, idrange_map(IdStart-SrcStart,Ranges,Mapped0),idrange_map(SrcEnd-IdEnd,Ranges,Mapped1), append(Mapped0,Mapped1,Mapped), (Start is SrcStart+Delta), (End is SrcEnd+Delta).

abs_maps(Maps0, Map) :- maplist(abs_map, Maps0, Map).
abs_map(Map0, Map) :- maplist(abs_range, Map0, Map).
abs_range([Dst,Src,Len], Delta-Src-End) :- End is Src+Len, Delta is Dst-Src.

map_ranges(StartRanges, Map, DestRanges) :- maplist(\Range^Mapped^( idrange_map(Range, Map, Mapped) ), StartRanges, MappedRanges), append(MappedRanges, DestRanges).

apply_maps(A,[],A).
apply_maps(StartRanges, [Map|Maps], DestRanges) :- map_ranges(StartRanges,Map,DestRanges0),apply_maps(DestRanges0,Maps,DestRanges).

min_ranges([S-_], S).
min_ranges([S-_|Rest], Min) :- min_ranges(Rest, Min0), Min is min(Min0,S).

part2(A) :-
    input(5,Input),
    phrase(almanac(Seeds,Maps0),Input),
    phrase(pairs(SeedRanges0),Seeds),
    maplist(\((Start-Len)^(Start-End)^(End is Start+Len)), SeedRanges0, SeedRanges),
    abs_maps(Maps0, Maps),
    once(apply_maps(SeedRanges, Maps, LocationRanges)),
    min_ranges(LocationRanges,MinLocation),
    A = MinLocation.

part2s(A) :-
    phrase_from_file(seq(Input), "inputs/sample5.txt"),
    phrase(almanac(Seeds,Maps0),Input),
    phrase(pairs(SeedRanges0),Seeds),
    maplist(\((Start-Len)^(Start-End)^(End is Start+Len)), SeedRanges0, SeedRanges),
    abs_maps(Maps0, Maps),
    once(apply_maps(SeedRanges, Maps, LocationRanges)),
    min_ranges(LocationRanges,MinLocation),
    A = MinLocation.

% use write_canonical(Expr). to check syntax.
