:- module(day6,[part1/1,part2/1]).

:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(lambda)).
:- use_module(library(charsio)).
:- use_module(library(debug)).

:- use_module(input).
:- use_module(day2).
:- use_module(day4).

zip([],[],[]).
zip([A|As],[B|Bs],[A-B|ABs]) :- zip(As,Bs,ABs).

race(Times, Records) --> "Time:",ws,nums(Times),"\n","Distance:",ws,nums(Records),"\n".

hold_distance(Total, Held, Distance) :- Distance is Held*(Total - Held).

distances_over_record(Time, Record, N) :- MinHeld is (1/2)*(Time - sqrt(Time*Time - 4*Record)), MaxHeld is (1/2)*(Time + sqrt(Time*Time - 4*Record)), N is 1+floor(MaxHeld)-floor(MinHeld+1).

part1(Product) :-
    input(6,Input),
    phrase(race(Times,Records), Input),
    zip(Times,Records,TRs),
    maplist(\((T-R)^N^distances_over_record(T,R,N)),TRs,Counts),
    foldl(\A^B^P^(P is A*B), Counts, 1, Product).

ws_digits([C|Cs]) --> ws,[C],{char_type(C,numeric)},ws_digits(Cs).
ws_digits([]) --> ws.

race2(Time, Record) --> "Time:",ws_digits(Time),"\n","Distance:",ws_digits(Record),"\n".

part2(Count) :-
    input(6,Input),
    phrase(race2(TimeC,RecordC), Input),
    number_chars(Time, TimeC), number_chars(Record,RecordC),
    distances_over_record(Time,Record,Count).
