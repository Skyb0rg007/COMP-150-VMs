#!/usr/bin/env swipl
% vim: ft=prolog

:- module(uft_main, []).
:- use_module(library(main)).
:- use_module(uft/parse, [parse/2]).
:- use_module(uft/unparse, [unparse_print/1]).
:- use_module(uft/label_elim, [transform/2 as label_elimination]).
:- use_module(uft/to_object, [to_object/2]).

:- initialization(main, main).

opts_spec(Spec) :-
    Spec = [
        [ opt(from), longflags([from]), default(vs), type(atom), help('Must be vs, what uft reads') ],
        [ opt(to), longflags([to]), default(vs), type(atom), help('One of vo or vs, what uft produces') ]
    ].

usage :-
    current_prolog_flag(executable, Arg0),
    atomic_list_concat(Arg0Parts, '/', Arg0),
    last(Arg0Parts, Arg0Last),
    format(atom(Msg), "Usage: ~a [--from vs] [--to [vo|vs]] [file]", [Arg0Last]),
    throw(error(Msg)).

main(Argv) :-
    opts_spec(Spec),
    opt_parse(Spec, Argv, [from(From), to(To)], PosArgs),
    (   (var(From) ; var(To))
    ->  usage
    ;   From-To = vs-vs, PosArgs = [File]
    ->  vs_vs(File)
    ;   From-To = vs-vo, PosArgs = [File]
    ->  vs_vo(File)
    ;   usage
    ),
    halt.

vs_vs(File) :-
    read_file_to_string(File, Str, []),
    parse(Str, Ast1),
    label_elimination(Ast1, Ast2),
    unparse_print(Ast2).

vs_vo(File) :-
    read_file_to_string(File, Str, []),
    parse(Str, Ast1),
    label_elimination(Ast1, Ast2),
    to_object(Ast2, ObjCode),
    writeln(ObjCode).

