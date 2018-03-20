%% module:  lib_worker
%% desc:    worker logic module
%% time:    18/03/16
%% author:  hh

-module(lib_worker).

-behaviour(gen_fsm).

-compile(export_all).

%% callback funcs
-export([
    handle_event/3,
    handle_info/3,
    handle_sync_event/4,
    code_change/4,
    terminate/3
    ]).

%% API
-export([
    create_workers/0,
    event/2
    ]).

-include("job.hrl").

-record(work, {
    id = 0,         %% 任务
    state = 0,      %% 0空闲|1干活中
    done_time = 0   %% 期限
    }).

%% 初始化worker进程
create_workers() ->
    do_create_workers(?WORKERS).

event(Id, Event) ->
    gen_fsm:send_event(misc:get_worker_process(Id), Event).

do_create_workers(0) -> ok;
do_create_workers(N) ->
    gen_fsm:start_link(?MODULE, [N], []),
    do_create_workers(N - 1).


start_link(N) ->
    gen_fsm:start_link(?MODULE, [N], []).

init([N]) ->
    process_flag(trap_exit, true),
    misc:register_pid(self(), N),
    {ok, resting, #work{}, 5000}.

%% 在休息状态后超时请求工作
resting(_, State) ->
    %% 模拟休息
    timer:sleep(3000),
    {next_state, requesting, State}.

%% 在请求工作状态下成功获得工作,进入工作状态
requesting({have_job, {Num, Task}}, State) ->
    #w_task{mfa_sign = MFA} = Task,
    [M, F, A] = get_mfa(MFA),
    apply(M, F, A),
    %% sleep模拟工作时耗
    {next_state, working, State#work{id = Num, state = ?DOING}}.

%% 工作
working(_Event, State = #work{id = N}) ->
    %% 发信息给job_center说明已完成工作
    job_center:job_done(N),
    {next_state, done, State#work{id = 0, state = ?REST}}.

%% 完成工作进入休息
done(_Event, State) ->
    {next_state, resting, State, 5000}.

handle_info(_Info, State, StateData) ->
    {next_state, State, StateData}.

handle_sync_event(_Event, _From, State, StateData) ->
    {next_state, State, StateData}.

handle_event(_, _, Data) ->
    {keep_state, Data}.

%% 回调
code_change(_Vsn, _StateName, _StateData, _Extra) -> 
    {ok, _StateName, _StateData}.

%% 回调
terminate(_Reason, _State, _Data) ->
    void.

%% ###############  internal functions  ##################

get_mfa(MFA) ->
    Mod = get_mod(MFA),
    Func = get_func(MFA),
    Args = get_args(MFA),
    [Mod, Func, Args].

get_mod(MFA) ->
    case opt(mod, MFA) of
        {ok, Mod} -> Mod;
        _         -> io
    end.

get_func(MFA) ->
    case opt(func, MFA) of
        {ok, Fun} -> Fun;
        _         -> format
    end.

get_args(MFA) ->
    case opt(args, MFA) of
        {ok, Args} -> Args;
        _          -> ["not args"]
    end.

opt(Op, [{Op, Vlaue}|_]) ->
    {ok, Vlaue};
opt(Op, [_|Options]) ->
    opt(Op, Options);
opt(_, []) ->
    false.
