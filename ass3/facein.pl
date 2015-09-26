facein([person(andrzej, [susan, ken]),
 person(ken, [andrzej, susan]),
 person(susan, [reed, jessica, jen, andrzej, ken]),
 person(reed, [tony, jessica]),
 person(jessica, [jen]),
 person(tony, []),
 person(jen, [susan, jessica, tony])]).

mymember(P, [P | _]).
mymember(P, [_ | Tail]) :- mymember(P, Tail).

friend([person(P1, Friends) | _], P1, P2) :- mymember(P2, Friends).
friend([_ | Tail], P1, P2) :- friend(Tail, P1, P2).

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

compareLists([], _).
compareLists([Head | Tail], List) :-
    mymember(Head, List), compareLists(Tail, List).

