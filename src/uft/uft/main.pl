% vim ft=prolog

:- module(uft_main, []).
:- use_module(library(main)).
:- use_module(uft/parse, [parse/2]).
:- use_module(uft/unparse, [unparse/2]).
:- use_module(uft/label_elim, [transform/2 as label_elimination]).
:- use_module(uft/to_object, [to_object/2]).

:- initialization(main, main).

opts_spec(Spec) :-
    Spec = [
    ].

usage :-
    current_prolog_flag(executable, Arg0),
    atomic_list_concat(Arg0Parts, '/', Arg0),
    last(Arg0Parts, Arg0Last),
    format(atom(Msg), "Usage: ~a [file]", [Arg0Last]),
    throw(error(Msg)).

main(Argv) :-
    catch(main_(Argv), Err, format('~w~n', [Err])),
    halt.

main_(Argv) :-
    opts_spec(Spec),
    opt_parse(Spec, Argv, _Opts, PositionalArgs),
    (PositionalArgs = [File], ! ; usage),
    read_file_to_string(File, Str, []),
    parse(Str, Ast1),
    label_elimination(Ast1, Ast2),
    % unparse(Ast2, Ast2Str),
    % format("~s~n", [Ast2Str]),
    to_object(Ast2, ObjAst),
    writeln(ObjAst),
    halt.

