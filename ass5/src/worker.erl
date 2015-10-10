-module(worker).

-export([init/1, wait/2, mapper/2]).

init(Master) ->
    {ok, wait, Master}.

wait({mapping, MapFun, Data}, Master) ->
    io:format("wait worker!: ~p~n", [Master]),
    {next_state, mapper, {MapFun, Data, Master}, 0}.

mapper(timeout, {MapFun, Data, Master}) ->
    io:format("mapper worker!: ~p~n", [{MapFun, Data, Master}]),
    gen_fsm:send_event(Master, {mapper_done, self(), MapFun(Data)}),
    {next_state, wait, Master}.

%% Internals
