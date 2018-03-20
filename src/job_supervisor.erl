-module(job_supervisor).

-behaviour(supervisor).

-export([init/1,start_link/0]).

start_link() ->
	supervisor:start_link({local,?MODULE}, ?MODULE, []).

init([]) ->
	{ok, {{one_for_one, 2, 50},
	[
    {job_center, {job_center, start_link, []}, permanent, 10000, worker, [job_center]}
	]}}.