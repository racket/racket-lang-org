#lang datalog

edge(a, b).
edge(b, c).
edge(c, d).
edge(d, a).

path(X, Y) :- edge(X, Y).
path(X, Y) :-
  edge(X, Z),
  path(Z, Y).

path(X, Y)?
