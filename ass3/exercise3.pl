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

palindromeHelp(List, List).
palindromeHelp([_ | List], List).
palindromeHelp([Head | X], Y) :- palindromeHelp(X, [Head | Y]).

palindrome(X) :- palindromeHelp(X, []).

duplicate([], []).
duplicate([A | X], [A, A | Y]) :- duplicate(X, Y).

compress([], []).
compress([A, A | X], [A | Y]) :- compress([A | X], [A | Y]).
compress([A | X], [A | Y]) :- compress(X, Y).

member([A | _], A).
member([A | X], B) :- member(X, B).