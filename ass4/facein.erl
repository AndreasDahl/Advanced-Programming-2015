-module(facein).
-export([start/1, add_friend/2, friends/1, test/0]).


start(Name) ->
    Pid = spawn(fun() -> loop(Name, []) end),
    {ok, Pid}.

add_friend(Pid, Fid) ->
    blocking(Pid, {add_friend, Fid}).

friends(Pid) ->
    blocking(Pid, {friends}).

test() ->
    {ok, P} = start(muf),
    add_friend(P, dahl).

% Private

blocking(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.

loop(Name, Friends) ->
    receive
        {From, {get_name}} ->
            From ! {self(), {name, Name}},
            loop(Name, Friends);
        {From, {friends}} ->
            From ! {self(), Friends},
            loop(Name, Friends);
        {From, {add_friend, Fid}} ->
            {name, FName} = blocking(Fid, {get_name}),
            From ! {self(), ok},
            loop(Name, [FName | Friends]);
        Else ->
            io:format("Message not handled!: ~p~n", Else),
            loop(Name, Friends)
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
