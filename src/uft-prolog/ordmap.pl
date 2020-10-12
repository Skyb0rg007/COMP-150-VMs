% vim ft=prolog

:- module(ordmap, []).

% empty(-Map:ordmap) is det.
% Map is the empty ordmap
empty(ordmap{}).

% lookup(+K, +Map:ordmap, -V) is semidet.
% Try to find the value associated with K in Map
% Fails if it doesn't exist
lookup(K, ordmap{ key: X, val: Y, left: L, right: R }, V) :-
    (   K == X -> V = Y
    ;   K @< X -> lookup(K, L, V)
    ;   K @> X -> lookup(K, R, V)
    ).

% insert(+K, +V, +Map1:ordmap, -Map2:ordmap) is semidet.
% Associates key K with value V
% Replaces value if it exists
insert(K, V, ordmap{}, ordmap{ key: K, val: V, left: ordmap{}, right: ordmap{} }) :- !.
insert(K, V, ordmap{ key: X, val: Y, left: L0, right: R }, ordmap{ key: X, val: Y, left: L, right: R }) :-
    K @< X,
    insert(K, V, L0, L), !.
insert(K, V, ordmap{ key: X, val: Y, left: L, right: R0 }, ordmap{ key: X, val: Y, left: L, right: R }) :-
    K @> X,
    insert(K, V, R0, R), !.
insert(K, V, ordmap{ key: X, val: _, left: L, right: R }, ordmap{ key: K, val: V, left: L, right: R }) :-
    K == X.

% kv(+Map:ordmap, -List:list) is det
% Returns a flat pair-list of keys and values in order
kv(ordmap{}, []).
kv(ordmap{ key: K, val: V, left: L, right: R }, Vals) :-
    kv(L, LVals),
    kv(R, RVals),
    append(LVals, [K-V|RVals], Vals).

:- begin_tests(ordmap).

test(lookup) :-
    Map = ordmap{ key: foo, val: bar, left: ordmap{}, right: ordmap{} },
    lookup(foo, Map, Val),
    Val = bar.

test(lookup_dot) :-
    Map = ordmap{ key: foo, val: bar, left: ordmap{}, right: ordmap{} },
    bar = Map.lookup(foo).

test(insert) :-
    empty(Map0),
    Map1 = Map0.insert(foo, bar),
    Map2 = Map1.insert(foo, bar),
    Map2.lookup(foo) = bar.

:- end_tests(ordmap).

