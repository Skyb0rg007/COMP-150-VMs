% vim ft=prolog

:- module(uft_unparse, [unparse_print/1, unparse/2]).

%! unparse(++Instrs, -Str:string) is det.
unparse(Instrs, Str) :- with_output_to(string(Str), unparse_print(Instrs)).

%! unparse_print(++Instrs) is det.
unparse_print(Instrs) :-
    must_be(ground, Instrs),
    maplist(print_instr(0), Instrs).

%! print_instr(+Indent:integer, ++Instr) is det.
% Arithmetic
print_instr(Indent, add(R1, R2, R3))  :- format("~*+    %~d := %~d + %~d~n",  [Indent, R1, R2, R3]).
print_instr(Indent, sub(R1, R2, R3))  :- format("~*+    %~d := %~d - %~d~n",  [Indent, R1, R2, R3]).
print_instr(Indent, mul(R1, R2, R3))  :- format("~*+    %~d := %~d * %~d~n",  [Indent, R1, R2, R3]).
print_instr(Indent, div(R1, R2, R3))  :- format("~*+    %~d := %~d / %~d~n",  [Indent, R1, R2, R3]).
print_instr(Indent, mod(R1, R2, R3))  :- format("~*+    %~d := %~d % %~d~n",  [Indent, R1, R2, R3]).
print_instr(Indent, idiv(R1, R2, R3)) :- format("~*+    %~d := %~d // %~d~n", [Indent, R1, R2, R3]).
print_instr(Indent, eq(R1, R2, R3))   :- format("~*+    %~d := %~d = %~d~n",  [Indent, R1, R2, R3]).
print_instr(Indent, lt(R1, R2, R3))   :- format("~*+    %~d := %~d < %~d~n",  [Indent, R1, R2, R3]).
print_instr(Indent, gt(R1, R2, R3))   :- format("~*+    %~d := %~d > %~d~n",  [Indent, R1, R2, R3]).
print_instr(Indent, leq(R1, R2, R3))  :- format("~*+    %~d := %~d <= %~d~n", [Indent, R1, R2, R3]).
print_instr(Indent, geq(R1, R2, R3))  :- format("~*+    %~d := %~d >= %~d~n", [Indent, R1, R2, R3]).
% Type predicates
print_instr(Indent, function_chk(R1, R2)) :- format("~*+    %~d := function? %~d~n", [Indent, R1, R2]).
print_instr(Indent, pair_chk(R1, R2))     :- format("~*+    %~d := pair? %~d~n",     [Indent, R1, R2]).
print_instr(Indent, symbol_chk(R1, R2))   :- format("~*+    %~d := symbol? %~d~n",   [Indent, R1, R2]).
print_instr(Indent, number_chk(R1, R2))   :- format("~*+    %~d := number? %~d~n",   [Indent, R1, R2]).
print_instr(Indent, boolean_chk(R1, R2))  :- format("~*+    %~d := boolean? %~d~n",  [Indent, R1, R2]).
print_instr(Indent, null_chk(R1, R2))     :- format("~*+    %~d := null? %~d~n",     [Indent, R1, R2]).
print_instr(Indent, nil_chk(R1, R2))      :- format("~*+    %~d := nil? %~d~n",      [Indent, R1, R2]).
% List functions
print_instr(Indent, car(R1, R2))      :- format("~*+    %~d := car %~d~n",      [Indent, R1, R2]).
print_instr(Indent, cdr(R1, R2))      :- format("~*+    %~d := cdr %~d~n",      [Indent, R1, R2]).
print_instr(Indent, cons(R1, R2, R3)) :- format("~*+    %~d := cons %~d %~d~n", [Indent, R1, R2, R3]).
% Control flow
print_instr(Indent, halt)            :- format("~*+    halt~n", [Indent]).
print_instr(Indent, error(R))        :- format("~*+    error %~d~n", [Indent, R]).
print_instr(Indent, goto(N))         :- integer(N), format("~*+    goto ~d~n", [Indent, N]).
print_instr(Indent, goto(Lbl))       :- string(Lbl), format("~*+    goto ~s~n", [Indent, Lbl]).
print_instr(Indent, if(R))           :- format("~*+    if %~d~n", [Indent, R]).
print_instr(Indent, deflabel(Lbl))   :- format("~*+~s:~n", [Indent, Lbl]).
% Misc
print_instr(Indent, print(R))     :- format("~*+    print %~d~n",       [Indent, R]).
print_instr(Indent, printu(R))    :- format("~*+    printu %~d~n",      [Indent, R]).
print_instr(Indent, println(R))   :- format("~*+    println %~d~n",     [Indent, R]).
print_instr(Indent, hash(R1, R2)) :- format("~*+    %~d := hash %~d~n", [Indent, R1, R2]).
% Literals
print_instr(Indent, loadliteral(R, Lit)) :- format("~*+    %~d := ", [Indent, R]), print_literal(Lit), format("~n").
% Functions
print_instr(Indent, loadfunction(R, Arity, Instrs)) :-
    format("~*+    %~d := function ~d {~n", [Indent, R, Arity]),
    Indent1 is Indent + 4,
    maplist(print_instr(Indent1), Instrs),
    format("~*+    }~n", [Indent]).

%! print_literal(++Lit) is det.
print_literal(num(N))    :- format("~w", [N]).
print_literal(str(S))    :- format("~p", [S]).
print_literal(true)      :- format("true").
print_literal(false)     :- format("false").
print_literal(emptylist) :- format("emptylist").
print_literal(nil)       :- format("nil").

:- begin_tests(uft_ast).

test(unparse1) :-
    unparse([add(1, 2, 3)], "    %1 := %2 + %3\n").

test(unparse2) :-
    unparse([deflabel(foo)], "foo:\n").

:- end_tests(uft_ast).
