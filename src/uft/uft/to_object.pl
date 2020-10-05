% vim ft=prolog

:- module(uft_to_object, [to_object/2]).

to_object(Ast, Output) :-
    must_be(list, Ast),
    maplist(to_object_instr, Ast, ObjAst),
    atomics_to_string(ObjAst, "\n", Output),
    format('Output = ~w~n', Output).

% to_object_instr(Instr, _) :-
    % format('Instr = ~w~n', [Instr]), fail.
to_object_instr(add(R1, R2, R3), S) :-
    format(string(S), "+ ~d ~d ~d", [R1, R2, R3]).
to_object_instr(sub(R1, R2, R3), S) :-
    format(string(S), "- ~d ~d ~d", [R1, R2, R3]).
to_object_instr(mul(R1, R2, R3), S) :-
    format(string(S), "* ~d ~d ~d", [R1, R2, R3]).
to_object_instr(mod(R1, R2, R3), S) :-
    format(string(S), "% ~d ~d ~d", [R1, R2, R3]).
to_object_instr(div(R1, R2, R3), S) :-
    format(string(S), "/ ~d ~d ~d", [R1, R2, R3]).
to_object_instr(idiv(R1, R2, R3), S) :-
    format(string(S), "// ~d ~d ~d", [R1, R2, R3]).
to_object_instr(eq(R1, R2, R3), S) :-
    format(string(S), "= ~d ~d ~d", [R1, R2, R3]).
to_object_instr(lt(R1, R2, R3), S) :-
    format(string(S), "< ~d ~d ~d", [R1, R2, R3]).
to_object_instr(gt(R1, R2, R3), S) :-
    format(string(S), "> ~d ~d ~d", [R1, R2, R3]).
to_object_instr(leq(R1, R2, R3), S) :-
    format(string(S), "<= ~d ~d ~d", [R1, R2, R3]).
to_object_instr(geq(R1, R2, R3), S) :-
    format(string(S), ">= ~d ~d ~d", [R1, R2, R3]).
to_object_instr(function_chk(R1, R2), S) :-
    format(string(S), "function? ~d ~d", [R1, R2]).
to_object_instr(pair_chk(R1, R2), S) :-
    format(string(S), "pair? ~d ~d", [R1, R2]).
to_object_instr(symbol_chk(R1, R2), S) :-
    format(string(S), "symbol? ~d ~d", [R1, R2]).
to_object_instr(number_chk(R1, R2), S) :-
    format(string(S), "number? ~d ~d", [R1, R2]).
to_object_instr(boolean_chk(R1, R2), S) :-
    format(string(S), "boolean? ~d ~d", [R1, R2]).
to_object_instr(null_chk(R1, R2), S) :-
    format(string(S), "null? ~d ~d", [R1, R2]).
to_object_instr(nil_chk(R1, R2), S) :-
    format(string(S), "nil? ~d ~d", [R1, R2]).
to_object_instr(car(R1, R2), S) :-
    format(string(S), "car ~d ~d", [R1, R2]).
to_object_instr(cdr(R1, R2), S) :-
    format(string(S), "cdr ~d ~d", [R1, R2]).
to_object_instr(cons(R1, R2, R3), S) :-
    format(string(S), "cons ~d ~d ~d", [R1, R2, R3]).
to_object_instr(halt, "halt").
to_object_instr(error(R), S) :- format(string(S), "error ~d", [R]).
to_object_instr(goto(N), S) :- format(string(S), "goto ~d", [N]).
to_object_instr(if(N), S) :- format(string(S), "if ~d", [N]).
to_object_instr(print(R), S) :- format(string(S), "print ~d", [R]).
to_object_instr(printu(R), S) :- format(string(S), "printu ~d", [R]).
to_object_instr(println(R), S) :- format(string(S), "println ~d", [R]).
to_object_instr(hash(R1, R2), S) :-
    format(string(S), "hash ~d ~d", [R1, R2]).
to_object_instr(loadliteral(R, Lit), S) :-
    to_object_lit(Lit, LitObj),
    format(string(S), "loadliteral ~d ~s", [R, LitObj]).
to_object_instr(loadfunction(R, Arity, Body), S) :-
    to_object(Body, BodyObj),
    length(Body, Len),
    format(string(S), ".load ~d function ~d ~d~n~s", [R, Arity, Len, BodyObj]).

to_object_lit(num(N), S) :- format(string(S), '~w', [N]).
to_object_lit(str(Str), S) :-
    string_length(Str, Len),
    string_codes(Str, Codes),
    atomics_to_string([string, Len | Codes], ' ', S).
to_object_lit(true, "true").
to_object_lit(false, "false").
to_object_lit(emptylist, "emptylist").
to_object_lit(nil, "nil").



