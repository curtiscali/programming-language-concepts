male(tony).
male(ethan).
male(chris).
male(ben).
male(nathan).

female(sim).
female(helen).
female(sophie).
female(olivia).

parents(helen, sim, tony).
parents(chris, sim, tony).
parents(ben, sim, tony).
parents(sophie, helen, ethan).
parents(olivia, helen, ethan).
parents(nathan, helen, ethan).

sister_of(X, Y) :- female(X), parents(X, A, B), parents(Y, A, B), X \= Y.

second([_ | [S | _]], S).

one([H | T]) :- H \= [], T = [].

insert(E, [], [E]).
insert(E, [H | T], [E | [H | T]]) :- E < H.
insert(E, [H | T], [H | S]) :- E >= H, insert(E, T, S).

insertion_sort([], []).
insertion_sort([A], [A]).
insertion_sort([H | T], S) :- insertion_sort(T, S2), insert(H, S2, S).

indexa(_, [], _, false).
indexa(E, [E | _], A, A).
indexa(E, [_ | T], A, N) :- A1 is A + 1, indexa(E, T, A1, N).

index(E, L, N) :- indexa(E, L, 0, N).
