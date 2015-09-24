facein([person(andrzej, [susan, ken]),
 person(ken, [andrzej]),
 person(susan, [reed, jessica, jen, andrzej]),
 person(reed, [tony, jessica]),
 person(jessica, [jen]),
 person(tony, []),
 person(jen, [susan, jessica, tony])]).

member(P, [P | _]).
member(P, [_ | Tail]) :- member(P, Tail).

friend([person(P1, Friends) | _], P1, P2) :- member(P2, Friends).
friend([_ | Tail], P1, P2) :- friend(Tail, P1, P2).

goodfriends(G, P1, P2) :- friend(G, P1, P2), friend(G, P2, P1).
