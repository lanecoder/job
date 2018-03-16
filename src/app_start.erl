-module(app_start).
-export([
    start/0,
    stop/0
    ]).

-define(APP, job).

%%开启应用程序
start()->
	application:start(?APP).

stop()->
    application:stop(?APP).