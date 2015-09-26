mymember(P, [P | _]).
mymember(P, [_ | Tail]) :- mymember(P, Tail).

friend([person(P1, Friends) | _], P1, P2) :- mymember(P2, Friends).
friend([_ | Tail], P1, P2) :- friend(Tail, P1, P2).

friendsOf([person(P1, Ret) | _], P1, Ret).
friendsOf([_ | Tail], P1, Ret) :- friendsOf(Tail, P1, Ret).

goodfriends(G, P1, P2) :- friend(G, P1, P2), friend(G, P2, P1).



% group G, person A, person-list B
% A is goodfriends with everyone in B
cliqueHelper(_, _, []).
cliqueHelper(G, A, [B | X]) :- goodfriends(G, A, B), cliqueHelper(G, A, X).

clique(_, []).
clique(G, [P | L]) :- cliqueHelper(G, P, L), clique(G, L).

% wannabe
listAll([], []).
listAll([person(Head, _)| Tail], [Head | X]) :- listAll(Tail, X).

containsAll([], _).
containsAll([Head | Tail], List) :-
    mymember(Head, List), containsAll(Tail, List).

addUnique(X, [], [X]).
addUnique(X, [X | Tail], [X | Tail]).
addUnique(X, [Head | Tail], [Head | Y]) :- addUnique(X, Tail, Y).

addUniques([], X, X).
addUniques([A | X], List, Ret) :-
    addUnique(A, List, ListAdded),
    addUniques(X, ListAdded, Ret).

createChain(_, [], _).
createChain(G, [Head | Tail], Ret) :-
    friendsOf(G, Head, X),
    addUniques(X, Tail, L),
    addUnique(Head, L, Ret).
