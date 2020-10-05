% vim ft=prolog

:- module(uft_transforms_label_elimination, [transform/2]).
:- use_module(library(clpfd)).
:- use_module(uft/ordmap, [empty/1 as ordmap_empty]).

transform(Instrs1, Instrs2) :-
    ordmap_empty(Env),
    transform_(Env, 0, Instrs1, Instrs2),
    !.

transform_(Env, N, [deflabel(Lbl)|Instrs1], Instrs2) :-
    N1 = Env.lookup(Lbl),
    (var(N1) ; throw(error('Duplicate label', Lbl, N1-N))),
    N = N1,
    !,
    transform_(Env, N, Instrs1, Instrs2).
transform_(Env, N, [deflabel(Lbl)|Instrs1], Instrs2) :-
    !,
    transform_(Env.insert(Lbl, N), N, Instrs1, Instrs2).
transform_(Env1, N, [goto(Lbl)|Instrs1], [goto(Offset)|Instrs2]) :-
    string(Lbl),
    (   LblPos = Env1.lookup(Lbl), Env2 = Env1, !
    ;   Env2 = Env1.insert(Lbl, LblPos)
    ),
    N1 #= N + 1,
    Offset #= LblPos - N1,
    !,
    transform_(Env2, N1, Instrs1, Instrs2).
transform_(Env1, N, [if_goto(R, Lbl)|Instrs1], [if(R), goto(Offset)|Instrs2]) :-
    (   LblPos = Env1.lookup(Lbl), Env2 = Env1, !
    ;   Env2 = Env1.insert(Lbl, LblPos)
    ),
    N1 #= N + 2,
    Offset #= LblPos - N1,
    !,
    transform_(Env2, N1, Instrs1, Instrs2).
transform_(Env, N, [loadfunction(R, Arity, FunInstrs1)|Instrs1], [loadfunction(R, Arity, FunInstrs2)|Instrs2]) :-
    transform(FunInstrs1, FunInstrs2),
    N1 #= N + 1,
    !,
    transform_(Env, N1, Instrs1, Instrs2).
transform_(Env, N, [Instr|Instrs1], [Instr|Instrs2]) :-
    N1 #= N + 1,
    transform_(Env, N1, Instrs1, Instrs2).
transform_(Env, _N, [], []) :-
    KVs = Env.kv(),
    findall(K-V, (member(K-V, KVs), var(V)), Undefs),
    (   Undefs = []
    ;   pairs_keys(Undefs, Keys),
        throw(error('Undefined labels', Keys, Keys))
    ).
