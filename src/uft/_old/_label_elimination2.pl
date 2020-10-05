% vim ft=prolog

:- module(uft_transforms_label_elimination2, [transform/2]).
:- use_module(library(clpfd)).

transform(Instrs, Instrs2) :-
    zip_instrs(Instrs, Instrs1, Env),
    ensure_nodups(Env),
    transform_instrs(Instrs1, Instrs2, Env).

transform_instrs([N-goto(Lbl)|Instrs1], [goto(Offset)|Instrs], Env) :-
    string(Lbl),
    (member(Lbl-LblPos, Env) ; throw(error('Undefined label', Lbl, N))),
    Offset #= LblPos - N - 1,
    !,
    transform_instrs(Instrs1, Instrs, Env).
transform_instrs([N-if_goto(R, Lbl)|Instrs1], [if(R), goto(Offset)|Instrs], Env) :-
    string(Lbl),
    (member(Lbl-LblPos, Env) ; throw(error('Undefined label', Lbl, N))),
    Offset #= LblPos - N - 2,
    !,
    transform_instrs(Instrs1, Instrs, Env).
transform_instrs([_N-Instr|Instrs1], [Instr|Instrs], Env) :-
    transform_instrs(Instrs1, Instrs, Env).
transform_instrs([], [], _Env).
transform_instrs([deflabel(_)|Instrs1], Instrs, Env) :-
    transform_instrs(Instrs1, Instrs, Env).

is_dup(X, Xs) :- select(X, Xs, Ys), member(X, Ys).
ensure_nodups(Env) :-
    pairs_keys(Env, EnvKeys),
    (   setof(K, is_dup(K, EnvKeys), DupKeys), DupKeys \= []
    ->  throw(error('Duplicate labels', DupKeys, Env))
    ;   true
    ).

% Turn 'instruction' into 'num-instruction',
% Collects the mapping from label to number
zip_instrs(Instrs, Instrs1, Env) :- foldl(zip_instr, Instrs, Instrs1, 0-[], _-Env).

zip_instr(deflabel(Lbl), deflabel(Lbl), N-Env, N-[Lbl-N|Env]) :- !.
zip_instr(if_goto(R, Lbl), N-if_goto(R, Lbl), N-Env, N1-Env) :- !, N1 #= N + 2.
zip_instr(Instr, Instr1, N-Env, N1-Env) :- N1 #= N + 1, Instr1 = N-Instr.

