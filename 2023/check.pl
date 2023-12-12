:- use_module(library(crypto)).

:- use_module(input).
:- use_module(day1).
:- use_module(day2).
% :- use_module(day3).
:- use_module(day4).
:- use_module(day5).

check("45a6bcaa93cb837edbd0a55d87bebcb17052759dadf995357bd173f58bb763e6a13094b67d93b4ad36978e2ae8357a2c") :-
    day1:part1(54632),day1:part2(54019),
    day2:part1(3035),day2:part2(66027),
    day4:part1(21138),day4:part2(7185540),
    day5:part1(57075758),day5:part2(31161857).

run :-
    cookie(Cookie),
    crypto_data_hash(Cookie,Hash,[algorithm(sha384)]),
    check(Hash).

:- initialization(run).
