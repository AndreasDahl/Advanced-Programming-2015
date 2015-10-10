-module(master).

-export([init/1, wait/2, map/2, terminate/3]).

init(Args) ->
    {ok, wait, Args}.

wait({job, NumWork, MapFun, RedFun, Initial, Data}, []) ->
    {ok, Worker} = gen_fsm:start({local, worker}, worker, self(), []),
    gen_fsm:send_event(Worker, {mapping, MapFun, Data}),
    io:format("wait Master: ~p~n", [Data]),
    {next_state, map, {start, NumWork, MapFun, RedFun, Initial, Data}}.

map({mapper_done, Worker, Result}, {start, NumWork, MapFun, RedFun, Initial, Data}) ->
    io:format("Result: ~p~n", [Result]),
    {next_state, wait, []}.

terminate(Reason, StateName, NewState) ->
    {stop, "stopped", []}.

%% Internals
