% vim ft=prolog

:- module(uft_parse_parser, [parse/2]).

%! parse(+Toks, -Ast) is det.
%  Parses the tokens into an AST, throws error on failure
parse(Toks, Ast) :-
    phrase(ast(Ast), Toks, Rest),
    (   Rest = [H|_]
    ->  throw(error('Invalid token', H, Rest))
    ;   true
    ).

%! ast(-Ast)// is semidet.
%  Parses SVM assembly
ast(Ast) --> separator, !, ast_(Ast).
ast(Ast) --> ast_(Ast).
ast_(Ast) --> line(L), separator, !, ast(Rest), { append(L, Rest, Ast) }.
ast_(L) --> line(L), !.
ast_([]) --> [].

%! separator// is semidet.
%  Parses separators used to separate assembly lines
separator --> ([;] ; ['\n']), separator.
separator --> ([;] ; ['\n']).

%! line(-Ast:list)// is semidet.
%  Parses a single line of assembly
line([deflabel(Lbl), Cmd]) --> [id(Lbl), :], command(Cmd).
line([deflabel(Lbl)]) --> [id(Lbl), :].
line([Cmd]) --> command(Cmd).

%! command(-Ast)// is semidet.
%  Parses a single command
% Arithmetic
command(add(R1, R2, R3))  --> [reg(R1), :=, reg(R2),  +,  reg(R3)].
command(sub(R1, R2, R3))  --> [reg(R1), :=, reg(R2),  -,  reg(R3)].
command(mul(R1, R2, R3))  --> [reg(R1), :=, reg(R2),  *,  reg(R3)].
command(div(R1, R2, R3))  --> [reg(R1), :=, reg(R2),  /,  reg(R3)].
command(mod(R1, R2, R3))  --> [reg(R1), :=, reg(R2), '%', reg(R3)].
command(idiv(R1, R2, R3)) --> [reg(R1), :=, reg(R2),  //, reg(R3)].
command(eq(R1, R2, R3))  --> [reg(R1), :=, reg(R2), =,  reg(R3)].
command(lt(R1, R2, R3))  --> [reg(R1), :=, reg(R2), <,  reg(R3)].
command(gt(R1, R2, R3))  --> [reg(R1), :=, reg(R2), >,  reg(R3)].
command(geq(R1, R2, R3)) --> [reg(R1), :=, reg(R2), >=, reg(R3)].
command(leq(R1, R2, R3)) --> [reg(R1), :=, reg(R2), <=, reg(R3)].
% Type predicates
command(function_chk(R1, R2)) --> [reg(R1), :=, function_chk, reg(R2)].
command(pair_chk(R1, R2))     --> [reg(R1), :=, pair_chk,     reg(R2)].
command(symbol_chk(R1, R2))   --> [reg(R1), :=, symbol_chk,   reg(R2)].
command(number_chk(R1, R2))   --> [reg(R1), :=, number_chk,   reg(R2)].
command(boolean_chk(R1, R2))  --> [reg(R1), :=, boolean_chk,  reg(R2)].
command(null_chk(R1, R2))     --> [reg(R1), :=, null_chk,     reg(R2)].
command(nil_chk(R1, R2))      --> [reg(R1), :=, nil_chk,      reg(R2)].
% List functions
command(car(R1, R2))      --> [reg(R1), :=, car, reg(R2)].
command(cdr(R1, R2))      --> [reg(R1), :=, cdr, reg(R2)].
command(cons(R1, R2, R3)) --> [reg(R1), :=, cons, reg(R2), reg(R3)].
% Control flow
command(halt)            --> [halt].
command(error(R))        --> [error, reg(R)].
command(goto(Lbl))       --> [goto, id(Lbl)].
command(goto(N))         --> [goto, num(N)], { integer(N) }. % No floats
command(if(R))           --> [if, reg(R)].
% Misc
command(print(R))     --> [print], [reg(R)].
command(printu(R))    --> [printu], [reg(R)].
command(println(R))   --> [println], [reg(R)].
command(hash(R1, R2)) --> [reg(R1), :=, hash, reg(R2)].
% Literals
command(loadliteral(R, Lit)) --> [reg(R), :=], literal(Lit).
% Functions
command(loadfunction(R, Arity, Instrs)) -->
    [reg(R), :=, function, num(Arity)], { integer(Arity) }, ['{'], ast(Instrs), ['}'].

%! literal(-Ast)// is semidet.
%  Parses a literal
literal(num(N))    --> [num(N)].
literal(str(S))    --> [str(S)].
literal(true)      --> [true].
literal(false)     --> [false].
literal(emptylist) --> [emptylist].
literal(nil)       --> [nil].

:- begin_tests(uft_parse_parser).

test(parse, true(Ast = [deflabel(foo), add(1, 2, 3)])) :-
    parse([id(foo), :, reg(1), :=, reg(2), +, reg(3)], Ast).

:- end_tests(uft_parse_parser).

