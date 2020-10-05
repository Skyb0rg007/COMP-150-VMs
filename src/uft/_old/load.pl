% vim: ft=prolog

:- module(load, [run_uft/0]).

:- use_module(library(unicode)).
:- use_module(library(optparse)).

opts_spec(Spec) :-
    Spec = [].

run_uft :-
    opts_spec(OptSpec),
    opt_arguments(OptSpec, _Opts, [Arg|_]),
    format('argv[0] = ~s~n', Arg),
    unicode_map(Arg, Arg1, [casefold]),
    format('argv[0] = ~s~n', Arg1).

