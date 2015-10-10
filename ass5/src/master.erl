-module(master).

-export([init/1, wait/2, terminate/3]).

init(Args) ->
    {ok, wait, Args}.

wait(New, Args) ->
    {next_state, wait, New ++ Args}.

terminate(Reason, StateName, NewState) ->
    {stop, "stopped", []}.
