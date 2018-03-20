-module(job_app).

-behaviour(application).

-export([
    start/2,
    stop/1]
    ).

start(_Type,_StartArg) ->
    io:format("app start...", []),
	job_supervisor:start_link().

stop(_State) ->
	ok.