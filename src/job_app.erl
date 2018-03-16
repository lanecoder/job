-module(ballgame_app).

-behaviour(application).

-export([
    start/2,
    stop/1]
    ).

start(_Type,_StartArg) ->
	job_supervisor:start_link().

stop(_State) ->
	ok.