-module(facein_test).
-export([test/0]).

-import(facein,[start/1, add_friend/2, friends/1, broadcast/3, received_messages/1]).
-import(timer,[sleep/1]).

test() ->
    {ok, Pken} = start(ken),
    {ok, Pandrzej} = start(andrzej),
    {ok, Psusan} = start(susan),
    {ok, Preed} = start(reed),
    {ok, Pjessica} = start(jessica),
    {ok, Ptony} = start(tony),
    {ok, Pjen} = start(jen),
    add_friend(Pken, Pandrzej),
    %add_friend(Pandrzej, Pken), % No one likes Ken :)
    add_friend(Pandrzej, Psusan),
    add_friend(Psusan, Pandrzej),
    add_friend(Psusan, Preed),
    add_friend(Psusan, Pjessica),
    add_friend(Psusan, Pjen),
    add_friend(Preed, Pjessica),
    add_friend(Preed, Ptony),
    add_friend(Pjessica, Pjen),
    add_friend(Pjen, Psusan),
    add_friend(Pjen, Pjessica),
    add_friend(Pjen, Ptony),
    R1 = friends(Pjen),
    broadcast(Preed, hello_dear_friends, 2),
    broadcast(Pken, "Why are you all ignoring me?", 3),
    sleep(500),
    R2 = received_messages(Ptony),
    R3 = received_messages(Psusan),
    R4 = received_messages(Preed),
    R5 = received_messages(Pjen),
    R6 = received_messages(Pken),
    R7 = received_messages(Pjessica),
    [R1, R2, R3, R4, R5, R6, R7].