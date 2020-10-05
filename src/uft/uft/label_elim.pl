% vim ft=prolog

:- module(uft_label_elim, [transform/2]).
:- use_module(library(clpfd)).
:- use_module(ordmap, [empty/1 as ordmap_empty]).

%! transform(+Ast1, -Ast) is det.
%  Eliminates labels, converting goto(lbl) into goto(offset).
transform(Ast1, Ast) :-
    ordmap_empty(Env),
    transform(Ast1, Ast, 0, Env).

% deflabel
% Lookup the label. If it exists and is bound, error.
% If it exists as a var, unify it.
% If it doesn't exist, bind it.
transform([deflabel(Lbl)|Ast1], Ast, N, Env) :-
    N1 = Env.lookup(Lbl),
    (var(N1), ! ; throw(error('Duplicate label', Lbl, N1-N))),
    N = N1,
    !,
    transform(Ast1, Ast, N, Env).
transform([deflabel(Lbl)|Ast1], Ast, N, Env) :-
    !,
    transform(Ast1, Ast, N, Env.insert(Lbl, N)).
% goto
% Lookup the label. If it exists, use the value to compute the offset.
% If it doesn't exist, bind a variable with the correct clpfd constraints.
transform([goto(Lbl)|Ast1], [goto(Offset)|Ast], N, Env1) :-
    string(Lbl),
    !,
    (   LblPos = Env1.lookup(Lbl),
        Env2 = Env1,
        !
    ;   Env2 = Env1.insert(Lbl, LblPos)
    ),
    N1 #= N + 1,
    Offset #= LblPos - N1,
    transform(Ast1, Ast, N1, Env2).
% loadfunction
% Recurse down the function body, using a fresh environment.
transform([loadfunction(R, Arity, Body1)|Ast1], [loadfunction(R, Arity, Body)|Ast], N, Env) :-
    !,
    transform(Body1, Body),
    N1 #= N + 1,
    transform(Ast1, Ast, N1, Env).
% Otherwise, do nothing
transform([Instr|Ast1], [Instr|Ast], N, Env) :-
    N1 #= N + 1,
    transform(Ast1, Ast, N1, Env).
% At the end, error if any label is not ground
transform([], [], _N, Env) :-
    KVs = Env.kv(),
    findall(K-V, (member(K-V, KVs), var(V)), Undefs),
    (   Undefs = [], !
    ;   pairs_keys(Undefs, Keys),
        throw(error('Undefined labels', Keys, Keys))
    ).

