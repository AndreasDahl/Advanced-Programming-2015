-module(facein).
-export([start/1, add_friend/2, friends/1, test/0, broadcast/3]).


start(Name) ->
    Pid = spawn(fun() -> loop(Name, [], []) end),
    {ok, Pid}.

add_friend(Pid, Fid) ->
    blocking(Pid, {add_friend, Fid}).

friends(Pid) ->
    blocking(Pid, {friends}).

broadcast(Pid, Msg, Radius) ->
    io:format("Broadcast: ~p~n", [Pid]),
    Pid ! {broadcast, Msg, Radius},
    ok.

received_messages(Pid) ->
    blocking(Pid, {received_messages}).

test() ->
    {ok, P1} = start(muf),
    {ok, P2} = start(dahl),
    add_friend(P1, P2),
    add_friend(P2, P1),
    broadcast(P1, "hej", 1).

% Private

broadcast_all([], _, _) -> ok;
broadcast_all([{Pid, _} | Friends], Msg, Radius) ->
    broadcast(Pid, Msg, Radius),
    broadcast_all(Friends, Msg, Radius).

blocking(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.

loop(Name, Friends, Messages) ->
    receive
        {From, {get_name}} ->
            From ! {self(), {name, Name}},
            loop(Name, Friends, Messages);
        {From, {friends}} ->
            From ! {self(), Friends},
            loop(Name, Friends, Messages);
        {From, {add_friend, Fid}} ->
            {name, FName} = blocking(Fid, {get_name}),
            From ! {self(), ok},
            loop(Name, [{Fid, FName} | Friends], Messages);
        {broadcast, Msg, 0} ->
            loop(Name, Friends, [Msg | Messages]);
        {broadcast, Msg, Radius} ->
            broadcast_all(Friends, Msg, Radius-1),
            loop(Name, Friends, [Msg | Messages]);
        {From, {received_messages}} ->
            From ! {self(), Messages},
            loop(Name, Friends, Messages);
        Else ->
            io:format("Message not handled!: ~p~n", Else),
            loop(Name, Friends, Messages)
    end.

% -export([start/0, add/2, list_all/1, update/2]).
%
% start() -> spawn(fun() -> loop(dict:new()) end).
%
% add(Pid, Contact) ->
%     blocking(Pid, {add, Contact}).
%
% list_all(Pid) ->
%     blocking(Pid, list_all).
%
% update(Pid, Contact) ->
%     blocking(Pid, {update, Contact}).
%
%
% % Internals
%
% blocking(Pid, Request) ->
%     Pid ! {self(), Request},
%     receive
%         {Pid, Response} -> Response
%     end.
%
% loop(Contacts) ->
%     receive
%         {From, {add, Contact}} ->
%             {Name,_,_} = Contact,
%             case dict:is_key(Name, Contacts) of
%                 false ->
%                     From ! {self(), ok},
%                     loop(dict:store(Name, Contact, Contacts));
%                 true ->
%                     From ! {self(), {error, Name, is_already_there}},
%                     loop(Contacts)
%             end;
%         {From, list_all} ->
%             List = dict:to_list(Contacts),
%             From ! {self(), {ok, lists:map(fun({_, C}) -> C end, List)}},
%             loop(Contacts);
%         {From, {update, Contact}} ->
%             {Name,_,_} = Contact,
%             NewContacts = dict:erase(Name, Contacts),
%             From ! {self(), ok},
%             loop(dict:store(Name, Contact, NewContacts));
%         {From, Other} ->
%             From ! {self(), {error,unknow_request, Other}},
%             loop(Contacts)
%     end.
