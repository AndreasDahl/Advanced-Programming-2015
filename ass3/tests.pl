:- consult(facein).

% Tests
test1 :-
    facein(G),
    goodfriends(G, ken, andrzej),
    goodfriends(G, andrzej, ken).

test2 :-
    facein(G),
    clique(G, [jessica, jen]),
    clique(G, [andrzej, susan, ken]).

test_all :- test1, test2.
