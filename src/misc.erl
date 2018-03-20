%%  Module  : misc
%%  Description: 公共函数
%%  Author  : hh
%%  time    : 18/03/16

-module(misc).
-compile(export_all).

process_name(N) ->
    list_to_atom(lists:concat(["worker_", integer_to_list(N)])).

register_pid(Pid, N) ->
    Name = process_name(N),
    erlang:register(Name, Pid).

unregister_pid(Name) ->
    erlang:unregister(Name).

get_worker_process(N) ->
    erlang:whereis(process_name(N)).

is_process_alive(Pid) ->
    try
        if
            is_pid(Pid) ->
                NodePid = node(Pid),
                case NodePid =:= node() of
                    true ->
                        erlang:is_process_alive(Pid);
                    false ->
                        case rpc:call(NodePid, erlang, is_process_alive, [Pid], 2000) of
                            {badrpc, _Reason}  -> false;
                            Ret -> Ret
                        end
                end;
            true -> false
        end
    catch
        _:_ -> false
    end.

unixtime() ->
    erlang:system_time(seconds).

send_to_id(Id, Msg) ->
    Pid = get_worker_process(Id),
    Pid ! Msg.

print(Module, Line, Args) ->
    io:format("~p ~p ~p ~n", [Module, Line, Args]).
