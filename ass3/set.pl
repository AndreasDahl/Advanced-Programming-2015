% Set operations
% a is subset of B (boolean)
setSubset1(A, [A | _]).
setSubset1(A, [_ | Tail]) :- setSubset1(A, Tail).

% A is subset of B (boolean)
setSubset([], _).
setSubset([Head | Tail], List) :-
    setSubset1(Head, List),
    setSubset(Tail, List).

% a union B gives C
setUnion1(X, [], [X]).
setUnion1(X, [X | Tail], [X | Tail]).
setUnion1(X, [Head | Tail], [Head | Y]) :- setUnion1(X, Tail, Y).

% A union B gives C
setUnion([], X, X).
setUnion([A | X], List, Ret) :-
    setUnion1(A, List, ListAdded),
    setUnion(X, ListAdded, Ret).

% A intersection B gives C
setIntersection([], _, []).
setIntersection(_, [], []).
setIntersection([Head|At], B, [Head|Ct]) :-
    setSubset1(Head, B),
    setIntersection(At, B, Ct).
setIntersection([_|At], B, C) :-
    setIntersection(At, B, C).

% A complement B gives C
setComplement(A, [], A).
setComplement(A, A, []).
setComplement([], _, []).
setComplement([Head|At], B, C) :-
    setSubset1(Head, B),
    setComplement(At, B, C).
setComplement([Head|At], B, [Head|Ct]) :-
    setComplement(At, B, Ct).
