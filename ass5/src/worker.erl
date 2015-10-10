-module(worker).

-export([init/1, wait/2, mapper/2, reducer/2, terminate/3]).

-import(lists, [map/2, foldl/3]).

init(Master) ->
    {ok, wait, Master}.

wait({mapping, MapFun, Data}, Master) ->
    io:format("wait worker!: ~p~n", [Master]),
    {next_state, mapper, {MapFun, Data, Master}, 0};
wait({reducing, RedFun, Initial, Data}, Master) ->
    io:format("wait worker!: ~p~n", [Master]),
    {next_state, reducer, {RedFun, Initial, Data, Master}, 0}.

mapper(timeout, {MapFun, Data, Master}) ->
    gen_fsm:send_event(Master, {mapper_done, self(), map(MapFun, Data)}),
    {next_state, wait, Master}.
    %io:format("mapper worker!: ~p~n", [{MapFun, Data, Master}]),
    %gen_fsm:send_event(Master, {mapper_done, self(), MapFun(Data)}),
    %{next_state, wait, Master}.

reducer(timeout, {RedFun, Initial, Data, Master}) ->
    io:format("reducer!: ~p~n", [{RedFun, Initial, Data}]),
    gen_fsm:send_event(Master, {reducer_done, self(), foldl(RedFun, Initial, Data)}),
    {next_state, wait, Master}.

terminate(_Reason, _StateName, _NewState) ->
    {stop, "work stopped", []}.

%% Internals
