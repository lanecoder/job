%% module: job_center
%% desc:   programing erlang chapter 22 test
%% time:   2018//03/16
%% author: hh


%% 任务中心调度主进程,

-module(job_center).

-behaviour(gen_server).

%% gen_server callbacks
-export([
    init/1, 
    handle_call/3, 
    handle_cast/2, 
    handle_info/2, 
    terminate/2, 
    code_change/3]).

%% API
-export([
    start_link/0,
    add_job/1,
    job_wanted/1,
    job_done/1
    ]).

-include("job.hrl").

-define(SERVER, ?MODULE).
-define(INTERVAL, 60 * 1000).

%%%-------------------------------------------------------------------
%%% API Functions
%%%-------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

%% 添加任务
add_job(F) when is_function(F) ->
    gen_server:call(?SERVER, {add_job, F});

add_job([{M, F, A}]) ->
    gen_server:call(?SERVER, {add_job, [{M, F, A}]}).

%% 请求任务
job_wanted(Id) ->
    gen_server:cast(?SERVER, {job_wanted, [Id]}).

%% 完成任务
job_done(N) ->
    gen_server:cast(?SERVER, {job_done, [N]}).

%%%-------------------------------------------------------------------
%%% Callback Functions
%%%-------------------------------------------------------------------
init([]) ->
    ok = lib_worker:create_workers(),
    erlang:send_after(?INTERVAL, self(), {check_jobs}), 
    io:format("init the job_center...", []),
    {ok, #job_queue{}}.

handle_call({add_job, F}, _From, State = #job_queue{w_list = List, reque_list = RequestList}) ->
    Task = add_new_task(F, List),
    TaskList = [Task|List],
    {NewTaskList, NewRequestList} = check_have_request(TaskList, RequestList),
    NewState = State#job_queue{w_list = NewTaskList, reque_list = NewRequestList},
    {reply, Task#w_task.number, NewState};

handle_call({add_job, [{M, F, A}]}, _From, State = #job_queue{w_list = List, reque_list = RequestList}) ->
    Task = add_new_task([{M, F, A}], List),
    TaskList = [Task|List],
    {NewTaskList, NewRequestList} = check_have_request(TaskList, RequestList),
    {reply, Task#w_task.number, State#job_queue{w_list = NewTaskList, reque_list = NewRequestList}};

handle_call(_Request, _From, State) ->
    {reply, {ok, 0}, State}.

handle_cast({job_wanted, [Id]}, State) ->
    #job_queue{w_list = List, reque_list = RequestList} = State,
    case check_job_rest(List, []) of
        {true, Task = #w_task{}} ->
            case check_request(Id, RequestList) of
                true ->
                    NewTask = Task#w_task{worker = Id, state = ?DOING},
                    NewRequestList = lists:keydelete(Id, RequestList),
                    lib_worker:event(Id, {have_job, {Task#w_task.number, Task}});
                _ -> %% 列表中还有人比他早请求
                    NewTask = Task,
                    NewRequestList = RequestList
            end,
            NewList = lists:keyreplace(Task#w_task.number, #w_task.number, List, NewTask),
            State1 = State#job_queue{w_list = NewList, reque_list = NewRequestList};
        _ -> %% 没有需要分配的任务,将请求添加到列表中
            NewRequestList = update_request_list(Id, RequestList),
            State1 = State#job_queue{reque_list = NewRequestList}
    end,
    {noreply, State1};

handle_cast({job_done, [N]}, State = #job_queue{w_list = List}) ->
    NewState =
    case lists:keyfind(N, #w_task.number, List) of
        Task = #w_task{} ->
            NewTask = Task#w_task{worker = 0, state = ?DONE},
            NewList = lists:keyreplace(N, #w_task.number, List, NewTask),
            State = State#job_queue{w_list = NewList};
        _ ->
            State
    end,
    {noreply, NewState};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info({check_jobs}, State) ->
    erlang:send_after(?INTERVAL, self(), {check_jobs}),
    {noreply, State};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%-------------------------------------------------------------------
%%% Internal Functions
%%%-------------------------------------------------------------------

add_new_task(F, List) when is_function(F) ->
    Num = create_new_num(List),
    #w_task{
        number = Num,
        time = misc:unixtime(),
        mfa_sign = [{mod, ?MODULE}, {func, F}, {args, []}]
    };
add_new_task([{M, F, A = [_|_]}], List) ->
    Num = create_new_num(List),
    #w_task{
        number = Num,
        time = misc:unixtime(),
        mfa_sign = [{mod, M}, {func, F}, {args, A}]
    }.

create_new_num([]) -> 1;
create_new_num(List = [#w_task{number = Num}|_]) ->
    Max = max_num(List, Num),
    Max + 1.

max_num([], Max) -> Max;
max_num([#w_task{number = N}|Rest], Max) ->
    NewMax = if N > Max -> N; true -> Max end,
    max_num(Rest, NewMax).

check_job_rest([], []) -> {false, 0};
check_job_rest([], Sofar = [_|_]) -> 
    {true, get_min(task, Sofar)};
check_job_rest([Task = #w_task{state = ?REST}|Rest], Sofar) ->
    check_job_rest(Rest, [Task|Sofar]);
check_job_rest([_|Rest], Sofar) ->
    check_job_rest(Rest, Sofar).

% update_request_list(Id, []) -> [{Id, misc:unixtime()}];
update_request_list(Id, List) ->
    case lists:keyfind(Id, 1, List) of
        {Id, _} ->
            List;
        _ ->
            [{Id, misc:unixtime()}|List]
    end.

check_request(Id, List) ->
    {Id1, _} = get_min(reque, List),
    if Id1 =:= Id -> true;
        true -> false
    end.

get_min(task, TaskList) ->
    NewList = lists:sort(fun(#w_task{time = T1}, #w_task{time = T2}) -> T1 < T2 end, TaskList),
    case NewList of
        [_|_] -> hd(NewList);
        _     -> []
    end;
get_min(reque, List) ->
    NewList = lists:sort(fun({_Id1, T1}, {_Id2, T2}) -> T1 < T2 end, List),
    case NewList of
        [_|_] -> hd(NewList);
        _     -> []
    end.

check_have_request(TaskList, []) ->
    {TaskList, []};
check_have_request(TaskList, RequestList) ->
    List1 = filter_doing_task(TaskList, []),
    Task = get_min(task, List1),
    case Task of
        #w_task{} ->
            case get_min(reque, RequestList) of
                {Id, _T} ->
                    NewTask = Task#w_task{state = ?DOING, worker = Id},
                    NewTaskList = lists:keyreplace(Task#w_task.number, #w_task.number, TaskList, NewTask),
                    NewRequestList = lists:keydelete(Id, RequestList),
                    lib_worker:event(Id, {have_job, {Task#w_task.number, Task}}),
                    {NewTaskList, NewRequestList};
                _ ->
                    {TaskList, RequestList}
            end;
        _ ->
            {TaskList, RequestList}
    end.

filter_doing_task([], Sofar) -> Sofar;
filter_doing_task([Task = #w_task{state = ?REST}|Rest], Sofar) ->
    filter_doing_task(Rest, [Task|Sofar]);
filter_doing_task([_|Rest], Sofar) -> 
    filter_doing_task(Rest, Sofar).