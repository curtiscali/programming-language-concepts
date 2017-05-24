d(X, X, 1).
d(C, X, 0) :- atomic(C), C \= X.
d(U + V, X, DU + DV) :- d(U, X, DU), d(V, X, DV).
d(U * V, X, U * DV + V * DU) :- d(U, X, DU), d(V, X, DV).

simp_sum(U, 0, U).
simp_sum(0, V, V).
simp_sum(U, V, E3) :- atomic(U), atomic(V), E3 is U + V.

simp_prod(U, 1, U).
simp_prod(1, V, V).
simp_prod(_, 0, 0).
simp_prod(0, _, 0).
simp_prod(U, V, E3) :- atomic(U), atomic(V), E3 is U * V.

simp(U + V, E2) :- atomic(U), simp(V, SV), simp_sum(U, SV, E2).
simp(U + V, E2) :- atomic(V), simp(U, SU), simp_sum(SU, V, E2).
simp(U + V, E2) :- atomic(U), atomic(V), simp_sum(U, V, E2).
simp(U + V, E2) :- simp(U, SU), simp(V, SV), simp_sum(SU, SV, E2).
simp(U * V, E2) :- atomic(U), simp(V, SV), simp_prod(U, SV, E2).
simp(U * V, E2) :- atomic(V), simp(U, SU), simp_prod(SU, V, E2).
simp(U * V, E2) :- atomic(U), atomic(V), simp_prod(U, V, E2).
simp(U * V, E2) :- simp(U, SU), simp(V, SV), simp_prod(SU, SV, E2).

quicksort([], []).
quicksort([H | T], S) :-
   partition(T, H, Less, Same, Greater),
   quicksort(Less, SLess),
   quicksort(Greater, SGreater),
   append(SLess, [H | Same], SGreater, S).

partition([], _, [], [], []).
partition([H | T], P, [H | LESS], SAME, GREATER) :-
    H < P,
    partition(T, P, LESS, SAME, GREATER).
partition([H | T], P, LESS, [H | SAME], GREATER) :-
    H = P,
    partition(T, P, LESS, SAME, GREATER).
partition([H | T], P, LESS, SAME, [H | GREATER]) :-
    H > P,
    partition(T, P, LESS, SAME, GREATER).

append([], L, L).
append([H | T], L, [H | A]) :- append(T, L, A).

append(L1, L2, L3, A) :- append(L2, L3, L23), append(L1, L23, A).

merge([], O2, O2).
merge(O1, [], O1).
merge([H1 | T1], [H2 | T2], [H2 | M]) :- H1 > H2, merge([H1 | T1], T2, M).
merge([H1 | T1], [H2 | T2], [H1 | M]) :- H1 < H2, merge(T1, [H2 | T2], M).
merge([H1 | T1], [H2 | T2], [H1 | M]) :- H1 = H2, merge(T1, [H2 | T2], M).

split([], [], []).
split([A], [A], []).
split([F | [S | T]], [F | H1], [S | H2]) :- split(T, H1, H2).

merge_sort([], []).
merge_sort([A], [A]).
merge_sort(L, S) :-
    split(L, EVENS, ODDS),
    merge_sort(EVENS, SORTED_EVENS),
    merge_sort(ODDS, SORTED_ODDS),
    merge(SORTED_EVENS, SORTED_ODDS, S).

bool(true).
bool(false).

judgeType(Gamma, N, int) :- number(N).

judgeType(Gamma, B, bool) :- bool(B).

judgeType(Gamma, X, T) :- atom(X), lookUp(X, Gamma, T).

judgeType(Gamma, if(M, N1, N2), T) :-
        judgeType(Gamma, M, bool),
        judgeType(Gamma, N1, T),
        judgeType(Gamma, N2, T).

judgeType(Gamma, let(X, M, N), T) :-
        judgeType(Gamma, M, T1),
        extend(Gamma, X, T1, Gamma2),
        judgeType(Gamma2, N, T).

judgeType(Gamma, letRec(Y, X, TX, TM, M, N), T) :-
        extend(Gamma, X, TX, Gamma2),
        extend(Gamma2, Y, arrow(TX, TM), Gamma3),
        judgeType(Gamma3, M, TM),
        extend(Gamma, Y, arrow(TX, TM), Gamma4),
        judgeType(Gamma4, N, T).

judgeType(Gamma, proc(X, T, M), arrow(T, TM)) :-
        extend(Gamma, X, T, Gamma2),
        judgeType(Gamma2, M, TM).

judgeType(Gamma, funcall(M, N), T) :-
        judgeType(Gamma, N, TN),
        judgeType(Gamma, M, arrow(TN, T)).

judgeType(Gamma, nil(T), list(T)).

judgeType(Gamma, list(T), list(T)).

judgeType(Gamma, isNil(M), bool) :- judgeType(Gamma, M, list(_)).

judgeType(Gamma, cons(M, N), list(T)) :-
	judgeType(Gamma, N, list(T)),
    judgeType(Gamme, M, T).

judgeType(Gamma, head(M), T) :- judgeType(Gamma, M, list(T)).

judgeType(Gamma, tail(M), list(T)) :- judgeType(Gamma, M, list(T)).


lookUp(X, [[X, Type] | Tail], Type) :- !.
lookUp(X, [[Y, T] | Tail], Type) :- 
    lookUp(X, Tail, Type).

extend(Gamma, X, T, [[X, T] | Gamma]).

initGamma([
    [eq, arrow(int, arrow(int, bool))],
    [prod, arrow(int, arrow(int, int))],
    [sum, arrow(int, arrow(int, int))],
    [diff, arrow(int, arrow(int, int))]
          ]).