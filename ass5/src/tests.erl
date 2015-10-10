-module(tests).
-import (mr, [start/0]).

-ifdef(TEST).
-include_lib("eunit/include/eunit.hrl").

simple_test() ->
    ?assert(ok == start()).

-endif.
