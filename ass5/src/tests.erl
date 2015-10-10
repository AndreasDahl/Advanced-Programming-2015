-module(tests).
-import (mr, [start/0, job/1]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    {ok, Pid} = mr:start(),
    mr:job(Pid).

-endif.
