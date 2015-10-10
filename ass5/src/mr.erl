-module(mr).

%% mr: mr library's entry point.

-export([start/0, job/1]).

-export([init/1, wait/2]).



%% API

start() ->
    gen_fsm:start({local, mr}, mr, [], []).

job(Pid) ->
    gen_fsm:send_event(Pid, []).


% job(Pid, NumWork, MapFun, RedFun, Initial, Data) ->
%     end.
%
% stop(Pid) ->
%     end.
%
% advanced_job(Pid, NumWork, MapFun, RedFun, Initial, Data) ->
%     end.

%% Internals

init(Args) ->
    {ok, wait, Args}.

wait(New, Args) ->
    {next_state, wait, New ++ Args}.






%% End of Module.
