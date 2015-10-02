-module(facein).
-export([start/1, add_friend/2, friends/1, test/0, broadcast/3, received_messages/1]).

-import(lists,[map/2, member/2]).

start(Name) ->
    Pid = spawn(fun() -> loop(Name, [], []) end),
    {ok, Pid}.

add_friend(Pid, Fid) ->
    blocking(Pid, {add_friend, Fid}).

friends(Pid) ->
    blocking(Pid, {friends}).

broadcast(Pid, Msg, Radius) ->
    broadcast_private(Pid, {make_ref(), Msg}, Radius).

received_messages(Pid) ->
    blocking(Pid, {received_messages}).

test() ->
    {ok, P1} = start(muf),
    {ok, P2} = start(dahl),
    add_friend(P1, P2),
    add_friend(P2, P1),
    broadcast(P1, "hej", 5),
    P1.

% Private

broadcast_private(Pid, Msg, Radius) ->
    Pid ! {broadcast, Msg, Radius},
    ok.

broadcast_all(_, _, 0) -> ok;
broadcast_all([], _, _) -> ok;
broadcast_all([{Pid, _} | Friends], Msg, Radius) ->
    broadcast_private(Pid, Msg, Radius),
    broadcast_all(Friends, Msg, Radius).

blocking(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.

extract(N, List) ->
    map(fun(X) -> element(N, X) end, List).

loop(Name, Friends, Messages) ->
    receive
        {From, {get_name}} ->
            From ! {self(), {name, Name}},
            loop(Name, Friends, Messages);

        {From, {friends}} ->
            From ! {self(), extract(2, Friends)},
            loop(Name, Friends, Messages);

        {From, {add_friend, Fid}} ->
            {name, FName} = blocking(Fid, {get_name}),
            From ! {self(), ok},
            loop(Name, [{Fid, FName} | Friends], Messages);

        {broadcast, Msg, Radius} ->
            case member(Msg, Messages) of
                false ->
                    broadcast_all(Friends, Msg, Radius-1),
                    loop(Name, Friends, [Msg | Messages]);
                true ->
                    loop(Name, Friends, Messages)
            end;

        {From, {received_messages}} ->
            From ! {self(), extract(2, Messages)},
            loop(Name, Friends, Messages);

        Else ->
            io:format("Message not handled!: ~p~n", Else),
            loop(Name, Friends, Messages)
    end.
