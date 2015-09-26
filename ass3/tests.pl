:- consult(facein).

facein([person(andrzej, [susan, ken]),
 person(ken, [andrzej, susan]),
 person(susan, [reed, jessica, jen, andrzej, ken]),
 person(reed, [tony, jessica]),
 person(jessica, [jen]),
 person(tony, []),
 person(jen, [susan, jessica, tony])]).

test_mymember :-
    mymember(a, [a]),
    mymember(d, [a,b,c,d,e]),
    not(mymember(a, [b,c,d])).

test_friend :-
    facein(G),
    friend(G, reed, tony),
    not(friend(G, tony, reed)).

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

test_containsAll :-
    facein(G),
    listAll(G, L),
    containsAll([ken, susan, reed], L),
    containsAll([andrzej, ken, susan, reed, jessica, tony, jen], L).

test_addUnique :-
    addUnique(a, [b, c], [b, c, a]),
    addUnique(a, [a, b, c], [a, b, c]),
    addUnique(a, [], [a]).

test_addUniques :-
    addUniques([], [a], [a]),
    addUniques([a,b], [c,d], X), containsAll([a,b,c,d], X),
    addUniques([a,b,c], [a,b], [a,b,c]).

test_all :- test_mymember,
            test_friend,
            test_goodfriends,
            test_clique,
            test_listAll,
            test_containsAll,
            test_addUnique,
            test_addUniques.
