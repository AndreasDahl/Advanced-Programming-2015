-module(mr).

-import(master, [init/1, wait/2, terminate/3]).

%% mr: mr library's entry point.

-export([start/0, job/1, stop/1]).


%% API

start() ->
    gen_fsm:start({local, master}, master, [], []).

job(Pid) ->
    gen_fsm:send_event(Pid, []).

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
