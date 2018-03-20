{application,job
[
	{description,"job_center and workers"},
	{vsn, "1.0"},
	{modules, [job_app, job_supervisor, job_center, lib_worker]},
	{applications, [kernel,stdlib]},
	{mod, {job_app, []}},
	{start_phases, []}
]
}