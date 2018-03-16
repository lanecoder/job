%% desc:  head file
%% time:  10/03/16
%% author:hh

-define(WORKERS, 5).
-define(REST,    0).
-define(DOING,   1).
-define(DONE,    2).

%% 任务列表
-record(job_queue, {
    w_list = [],        %% 任务列表
    reque_list = []     %% 请求列表
    }).

%% 任务
-record(w_task, {
    number = 0,         %% 任务id
    worker = 0,         %% 工人编号
    time = 0,           %% 任务创建时间
    state = 0,          %% 当前状态 0未进行|1进行中|2已完成(可销毁)
    mfa_sign = []       %% 任务
    }).