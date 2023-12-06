
:- use_module(day1).
:- use_module(day2).
% :- use_module(day3).
:- use_module(day4).

run :-
    day1:part1(54632),day1:part2(54019),
    day2:part1(3035),day2:part2(66027),
    day4:part1(21138),day4:part2(7185540).

:- initialization(run).
