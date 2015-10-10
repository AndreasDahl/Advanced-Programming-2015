-module(mr).

%% mr: mr library's entry point.

-export([start/0, job/6, stop/1, test_sum/0]).


%% API

start() ->
    gen_fsm:start({local, master}, master, [], []).

job(Pid, NumWork, MapFun, RedFun, Initial, Data) ->
    {ok, gen_fsm:sync_send_event(Pid, {job, NumWork, MapFun, RedFun, Initial, Data})}.
    % return {ok, Result} if it succeeds.

stop(Pid) ->
    gen_fsm:stop(Pid).

test_sum() ->
    {ok, MR}  = mr:start(),
    {ok, Sum} = mr:job(MR,
                       3,
                       fun(X) -> X end,
                       fun(X, Acc) -> X + Acc end,
                       0,
                       lists:seq(1,10)),
    {ok, Fac} = mr:job(MR,
                       4,
                       fun(X) -> X end,
                       fun(X, Acc) -> X * Acc end,
                       1,
                       lists:seq(1,10)),
    mr:stop(MR),
    {Sum, Fac}.

% job(Pid, NumWork, MapFun, RedFun, Initial, Data) ->
%     end.
%
% stop(Pid) ->
%     end.
%
% advanced_job(Pid, NumWork, MapFun, RedFun, Initial, Data) ->
%     end.

%% Internals







%% End of Module.
