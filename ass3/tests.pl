:- consult(facein).

% Tests
test_goodfriends :-
    facein(G),
    goodfriends(G, ken, andrzej),
    goodfriends(G, andrzej, ken).

test_clique :-
    facein(G),
    clique(G, [jessica, jen]),
    clique(G, [andrzej, susan, ken]),
    not(clique(G, [jessica, jen, susan])).

test_listAll :-
    facein(G),
    listAll(G, [andrzej, ken, susan, reed, jessica, tony, jen]).

test_all :- test_goodfriends,
            test_clique,
            test_listAll.
