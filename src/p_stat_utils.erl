-module(p_stat_utils).
-export([get_short_stat_group/1,
         get_long_stat_group/1,
         get_history_group/1,
         register_short_server/2,
         register_long_server/2,
         register_history_server/2,
         unregister_short_server/2,
         unregister_long_server/2,
         unregister_history_server/2]).

-define(SHORT(Host), {?MODULE, short, Host}).
-define(LONG(Host), {?MODULE, long, Host}).
-define(HISTORY(Host), {?MODULE, history, Host}).

get_short_stat_group(Host) ->
    ?SHORT(Host).
get_long_stat_group(Host) ->
    ?LONG(Host).
get_history_group(Host) ->
    ?HISTORY(Host).

register_short_server(Host, Pid) ->
    pg2:join(?SHORT(Host), Pid).

unregister_short_server(Host, Pid) ->
    pg2:leave(?SHORT(Host), Pid).
    
register_long_server(Host, Pid) ->
    pg2:join(?LONG(Host), Pid).

unregister_long_server(Host, Pid) ->
    pg2:leave(?LONG(Host), Pid).

register_history_server(Host, Pid) ->
    pg2:join(?HISTORY(Host), Pid).

unregister_history_server(Host, Pid) ->
    pg2:leave(?HISTORY(Host), Pid).
