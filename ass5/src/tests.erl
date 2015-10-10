-module(tests).
-import (mr, [start/0, job/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

start_return_test() ->
    {ok, Pid} = mr:start(),
    mr:stop(Pid).

simple_job_test() ->
    {ok, Master} = mr:start(),
    mr:job(Master, 1, fun plus1/1, fun plus1/1, initial, 2).

plus1(A) -> A + 1.

-endif.
