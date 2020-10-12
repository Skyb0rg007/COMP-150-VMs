% vim ft=prolog

:- module(uft_parse, [parse/2]).
:- use_module(uft/parse/lexer, [tokenize/2]).
:- use_module(uft/parse/parser, [parse/2 as parser_parse]).

%! parse(+Str, -Ast) is det.
%  Throws error on parse failure
parse(Str, Ast) :-
    tokenize(Str, Toks),
    parser_parse(Toks, Ast).

