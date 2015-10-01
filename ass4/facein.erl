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

loop() ->
    receive
        {From, Request} ->
            From ! {self(), ComputeResult Request},
            loop();
        {From, Other} ->
            From ! {self(), {error,Other}},
            loop()
    end.
