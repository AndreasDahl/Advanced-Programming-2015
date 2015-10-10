-module(mr).

%% mr: mr library's entry point.

-export([start/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").
-endif.

%% API

start() ->
    ok.

% job(Pid, NumWork, MapFun, RedFun, Initial, Data) ->
%     end.
%
% stop(Pid) ->
%     end.
%
% advanced_job(Pid, NumWork, MapFun, RedFun, Initial, Data) ->
%     end.

%% Internals



%% Unit Tests.

-ifdef(TEST).

simple_test() ->
    ?assert(ok == start()).

-endif.

%% End of Module.
