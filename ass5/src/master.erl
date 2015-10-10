-module(master).

-export([init/1, wait/3, init2/2, map/2, reduce/2, terminate/3]).

-import(lists, [append/2, split/2]).

init(Args) ->
    {ok, wait, Args}.

wait({job, NumWork, MapFun, RedFun, Initial, Data}, Caller, []) ->
    {next_state, init2, {initialize, Caller, NumWork, MapFun, RedFun, Initial, Data}, 0}.

init2(timeout, {initialize, Caller, NumWork, MapFun, RedFun, Initial, Data}) ->
    [Reducer | Mappers] = start_workers(self(), NumWork),
    SplitData = split_data(Data, NumWork-1, length(Data) div (NumWork-1)),
    assign_mappers(Mappers, MapFun, SplitData),
    {next_state, map, {map, Caller, Reducer, NumWork-1, RedFun, Initial, []}}.
    %{ok, Worker} = gen_fsm:start({local, worker}, worker, self(), []),
    %gen_fsm:send_event(Worker, {mapping, MapFun, Data}),
    %io:format("wait Master: ~p~n", [Data]),
    %{next_state, map, {start, NumWork, MapFun, RedFun, Initial, Data}}.

map({mapper_done, _Worker, Result}, {map, Caller, Reducer, MapperCount, RedFun, Initial, Results}) ->
    io:format("result!: ~p~n", [Result]),
    case MapperCount of
        1 ->
            gen_fsm:send_event(Reducer, {reducing, RedFun, Initial, append(Result, Results)}),
            {next_state, reduce, {reduce, Caller}};
        _ ->
            {next_state, map, {map, Caller, Reducer, MapperCount-1, RedFun, Initial, append(Result, Results)}}
    end.
    %io:format("Result: ~p~n", [Result]),
    %{next_state, wait, []}.

reduce({reducer_done, _Worker, Result}, {reduce, Caller}) ->
    io:format("Result: ~p~n", [Result]),
    gen_fsm:reply(Caller, [Result]),
    {next_state, wait, []}.


terminate(_Reason, _StateName, _NewState) ->
    {stop, "stopped", []}.

%% Internals

start_workers(_, 0) -> [];
start_workers(Master, NumWork) ->
    {ok, Worker} = gen_fsm:start(worker, Master, []),
    [Worker | start_workers(Master, NumWork-1)].

split_data(Data, 1, _) -> [Data];
split_data(Data, N, PartSize) ->
    {Part, Rest} = split(PartSize, Data),
    [Part | split_data(Rest, N-1, PartSize)].

assign_mappers([], _, _) -> ok;
assign_mappers([Worker | RestOfWorkers], MapFun, [Data | RestOfData]) ->
    gen_fsm:send_event(Worker, {mapping, MapFun, Data}),
    assign_mappers(RestOfWorkers, MapFun, RestOfData).
