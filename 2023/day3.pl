:- use_module(library(dcgs)).
:- use_module(library(lists)).
:- use_module(library(lambda)).
:- use_module(library(format)).
:- use_module(library(charsio)).

:- use_module(library(debug)).

:- use_module(input).
:- use_module(day1).


write_lines([]).
write_lines([L|Lines]) :-
    format("~s",[L]),nl,
    write_lines(Lines).

% sublist(S,L) :- append([_,S,_], L).
% sublist(N,S,L) :- length(S,N),sublist(S,L).

% sublists(N,L,Ls) :- findall(S,(length(S,N),N>0,append([_,S,_], L)),Ls).
% sublists2(N,Ls,Lss) :- findall(S,(length(S,N),N>0,phrase((...,seq(S),...),Ls)),Lss).

line_symbols(Line,Symbols) :-
    maplist(\C^S^((char_type(C,numeric);C='.') -> S='.' ; S=(*)), Line, Symbols).

part1(A) :-
    input(3,Input),
    phrase(lines(Lines),Input),
    maplist(line_symbols,Lines,SymbolLines),
    maplist(\Ls^ALs^phrase(adj(ALs),Ls), SymbolLines, AdjSymbolLines),
    A = AdjSymbolLines.

adj(['.'|As],['.','.'|Bs])

% symbol_adjacent(Symbols,Adj) :-
