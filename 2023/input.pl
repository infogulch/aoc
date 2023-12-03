:- module(input, [input/2]).

:- use_module(library(http/http_open)).
:- use_module(library(si)).
:- use_module(library(format)).
:- use_module(library(pio)).
:- use_module(library(dcgs)).
:- use_module(library(charsio)).
:- use_module(library(files)).

input(N, Input) :-
    integer_si(N),
    (directory_exists("inputs") -> true ; make_directory("inputs")),
    phrase(format_("inputs/~w.txt", [N]), CachedInputFile),
    (file_exists(CachedInputFile) -> phrase_from_file(seq(Input), CachedInputFile)
    ; phrase(format_("https://adventofcode.com/2023/day/~w/input", [N]), Url),
    phrase_from_file(seq(Cookie), "cookie.txt"),
    http_open(Url, Response, [request_headers(['Cookie'(Cookie)])]),
    get_n_chars(Response,_Size,Input),
    phrase_to_file(seq(Input),CachedInputFile)
    ).
