:- module(util, [
    chars_type//2,
    num//1,
    nnum//1,
    nums//1,
    nnums//1,
    pairs//1,
    ws//0,
    zip/3
]).

:- use_module(library(charsio)).
:- use_module(library(reif)).

chars_type([C|Cs],Type) --> [C],{char_type(C,Type)},chars_type(Cs,Type).
chars_type("",_) --> "".

num(N) --> chars_type(Cs,numeric),{Cs=[_|_],number_chars(N,Cs)}.
nnum(N) --> "-",num(N0),{N is -N0}.
nnum(N) --> num(N).
nums([N|Ns]) --> num(N),ws,(" ",nums(Ns);{Ns=[]}).
nnums([N|Ns]) --> nnum(N),ws,(" ",nnums(Ns);{Ns=[]}).

pairs([A-B|Ps]) --> [A,B],pairs(Ps).
pairs([]) --> [].

ws --> "";" ",ws.

zip([],[],[]).
zip([A|As],[B|Bs],[A-B|ABs]) :- zip(As,Bs,ABs).

tall(If_1, Ls0, T) :- tfilter(If_1,Ls0,Ls1),length(Ls0,N0),length(Ls1,N1),=(N0,N1,T).
