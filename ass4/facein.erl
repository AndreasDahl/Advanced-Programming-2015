start() -> spawn(fun loop/0).

start() -> spawn(fun() -> loop(dict:new()) end).

add(Pid, Contact) ->
    blocking(Pid, {add, Contact}).

list_all(Pid) ->
    blocking(Pid, list_all).

update(Pid, Contact) ->
    blocking(Pid, {update, Contact}).

blocking(Pid, Request) ->
    Pid ! {self(), Request},
    receive
        {Pid, Response} -> Response
    end.

loop(Contacts) ->
    receive
        {From, {add, Contact}} ->
            {Name,_,_} = Contact,
            case dict:is_key(Name, Contacts) of
                false ->
                    From ! {self(), ok},
                    loop(dict:store(Name, Contact, Contacts));
                true ->
                    From ! {self(), {error, Name, is_already_there}},
                    loop(Contacts)
            end;
        {From, list_all} ->
            List = dict:to_list(Contacts),
            From ! {self(), {ok, lists:map(fun({_, C}) -> C end, List)}},
            loop(Contacts);
        {From, {update, Contact}} ->
            {Name,_,_} = Contact,
            NewContacts = dict:erase(Name, Contacts),
            From ! {self(), ok},
            loop(dict:store(Name, Contact, NewContacts));
        {From, Other} ->
            From ! {self(), {error,unknow_request, Other}},
            loop(Contacts)
    end
