parent(bob, carl).
parent(alice, bob).

parent(bob, dave).
parent(dave, eve).

ugly(bob).

grand(X, Y) :-
    parent(X, Z),
    parent(Z, Y).