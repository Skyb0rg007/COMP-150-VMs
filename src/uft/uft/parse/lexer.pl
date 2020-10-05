% vim ft=prolog

:- module(uft_parse_lexer, [tokenize/2]).
:- use_module(library(clpfd)).

%! tokenize(+Str:string, -Toks).
%  Tokenizes 'Str', throws an error on failure
tokenize(Str, Toks) :-
    string_codes(Str, Codes),
    phrase(tokens(Toks), Codes, Rest),
    (   Rest = [H|_]
    ->  string_codes(RestStr, Rest),
        char_code(HChar, H),
        throw(error('Unrecognized token', HChar, RestStr))
    ;   true
    ).

%! tokens(-Toks)// is semidet.
tokens([Tok | Rest]) --> token(Tok), !, tokens(Rest).
tokens(Toks) --> spaces1, !, tokens(Toks).
tokens([; | Toks]) --> comment, !, tokens(Toks).
tokens([]) --> [].

%! token(-Tok)// is semidet.
token(T) -->
    ident(Name),
    { keyword(Name, T), ! ; T = id(Name) }.
token(reg(R)) --> reg(R), \+ ident_start(_).
token(num(N)) --> num(N), \+ ident_start(_).
token(str(S)) --> str(S), \+ ident_start(_).
token(+)    --> o("+").
token(-)    --> o("-").
token(//)   --> o("//").
token(/)    --> o("/").
token(*)    --> o("*").
token(=)    --> o("=").
token(<=)   --> o("<=").
token(>=)   --> o(">=").
token(<)    --> o("<").
token(>)    --> o(">").
token('%')  --> o("%").
token(:=)   --> o(":=").
token('(')  --> o("(").
token(')')  --> o(")").
token('{')  --> o("{").
token('}')  --> o("}").
token(;)    --> o(";").
token(:)    --> o(":").
token('\n') --> o("\n").

%! keyword(+Str:string, -Atom:atom)// is semidet.
keyword("abs", abs).
keyword("boolean?", boolean_chk).
keyword("car", car).
keyword("cdr", cdr).
keyword("cons", cons).
keyword("emptylist", emptylist).
keyword("error", error).
keyword("false", false).
keyword("function", function).
keyword("function?", function_chk).
keyword("goto", goto).
keyword("halt", halt).
keyword("hash", hash).
keyword("idiv", idiv).
keyword("if", if).
keyword("nil", nil).
keyword("nil?", nil_chk).
keyword("null?", null_chk).
keyword("number?", number_chk).
keyword("pair?", pair_chk).
keyword("println", println).
keyword("print", print).
keyword("printu", printu).
keyword("symbol?", symbol_chk).
keyword("true", true).

%! comment// is semidet.
comment --> "#", comment_cont, "\n".
comment_cont --> [C], { C \= 0'\n }, !, comment_cont.
comment_cont --> [].

%! reg(-R:integer)// is semidet.
reg(R) --> "%", int(R).

%! ident(-Str:string)// is semidet
ident(Name) -->
    ident_start(H),
    ident_conts(T),
    { string_codes(Name, [H|T]) }.

%! ident_start(-C:integer)// is semidet.
ident_start(C) --> [C], { ident_start(C) }.
ident_conts([H|T]) --> [H], { ident_cont(H) }, !, ident_conts(T).
ident_conts([]) --> [].

%! ident_start(+C:integer) is semidet.
%  Succeeds if C is a character code that can be the first character of an identifier
ident_start(C) :-
    Lower = 0'a .. 0'z,
    Upper = 0'A .. 0'Z,
    Misc  = 0'_,
    C in Lower \/ Upper \/ Misc.

%! ident_cont(+C:integer) is semidet.
%  Succeeds if C is a character code that can be a part of an identifier
ident_cont(C) :-
    Lower = 0'a .. 0'z,
    Upper = 0'A .. 0'Z,
    Misc  = 0'_ \/ 0'- \/ 0'? \/ 0'!,
    C in Lower \/ Upper \/ Misc.

%! o(+Str:string)// is semidet
o(Name) -->
    { string_codes(Name, Codes) },
    codes(Codes).

%! codes(-Codes) is nondet.
codes([]) --> [].
codes([H|T]) --> [H], codes(T).

%! space// is semidet.
space --> [C], { nonvar(C), code_type(C, space) }.

%! spaces// is det.
spaces --> space, !, spaces.
spaces --> [].

%! spaces1// is semidet.
spaces1 --> space, !, spaces.

%! digit(-C:integer)// is semidet.
digit(C) --> [C], { code_type(C, digit) }.

%! digits(-Codes:list(integer))// is det.
digits([H|T]) --> digit(H), !, digits(T).
digits([]) --> [].

%! int(-I:integer)// is semidet.
%  Parses an integer with possible sign. Greedy.
int(I) -->
    int_(Codes),
    { number_codes(I, Codes) }.
int_([C,D0|D]) --> sign(C), !, digit(D0), digits(D).
int_([D0|D]) --> digit(D0), digits(D).

%! sign(-C:integer)// is semidet.
sign(0'-) --> "-".
sign(0'+) --> "+".

%! num(-N:number)// is semidet.
%  Parses an integer or floating-point number. Greedy.
num(N) -->
    int_(I),
    (   ".",
        digit(DF0),
        digits(DF)
    ->  { F = [0'., DF0 | DF] }
    ;   { F = [] }
    ),
    (   ("e" ; "E")
    ->  int_(DI),
        { E = [0'e | DI] }
    ;   { E = [] }
    ),
    { append([I, F, E], Codes), number_codes(N, Codes) }.

%! str(-S:string)// is semidet.
%  Parses a double-quote separated string, converting escape sequences.
str(S) -->
    "\"",
    str_(Codes),
    "\"",
    { string_codes(S, Codes) }.
str_([0'\n|T]) --> "\\n", !, str_(T).
str_([0'\t|T]) --> "\\t", !, str_(T).
str_([0'\v|T]) --> "\\v", !, str_(T).
str_([0'\\|T]) --> "\\\\", !, str_(T).
str_([C|T]) --> "\\", [C], !, str_(T).
str_([C|T]) --> [C], { C \= 0'" }, !, str_(T).
str_([]) --> [].

:- begin_tests(uft_parse_lexer).

test(tokenize, true(Toks = [])) :-
    tokenize(" \t\v\f", Toks).

test(tokenize, true(Toks = [goto, if])) :-
    tokenize("goto\tif", Toks).

:- end_tests(uft_parse_lexer).

