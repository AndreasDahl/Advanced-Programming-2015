-module(mr).

%% mr: mr library's entry point.

-export([start/0, job/6, stop/1]).


%% API

start() ->
    gen_fsm:start({local, master}, master, [], []).

job(Pid, NumWork, MapFun, RedFun, Initial, Data) ->
    gen_fsm:send_event(Pid, {job, NumWork, MapFun, RedFun, Initial, Data}).

stop(Pid) ->
    gen_fsm:stop(Pid).


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
