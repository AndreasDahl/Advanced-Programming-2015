append([], X, X).
append([X|Y], Z, [X|W]) :- append(Y, Z, W).

%reverse
/* Bad solution
last([Last | []], Last).
last([_ | Tail], Last) :- last(Tail, Last).

headL([_], []).
headL([Head | Tail], [Head | Return]) :- headL(Tail, Return).

reverse([], []).
reverse([Head | Tail], Out) :- last(Out, Head), headL(Out, X), reverse(Tail, X).
*/

% Solution by the help of Oleks
reverseHelp([], Ret, Ret).
reverseHelp([Head | Tail], X, Y) :- reverseHelp(Tail, [Head | X], Y).

reverse2(X, Y) :- reverseHelp(X, [], Y).
