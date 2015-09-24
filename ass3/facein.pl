facein([person(andrzej, [susan, ken]),
 person(ken, [andrzej]),
 person(susan, [reed, jessica, jen, andrzej]),
 person(reed, [tony, jessica]),
 person(jessica, [jen]),
 person(tony, []),
 person(jen, [susan, jessica, tony])]).

member(P, [P | _]).
member(P, [_ | Tail]) :- member(P, Tail).
