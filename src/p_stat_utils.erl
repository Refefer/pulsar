%% Copyright (c) 2012, Andrew Stanton <Refefer@gmail.com>
%% 
%% Permission to use, copy, modify, and/or distribute this software for any
%% purpose with or without fee is hereby granted, provided that the above
%% copyright notice and this permission notice appear in all copies.
%% 
%% THE SOFTWARE IS PROVIDED "AS IS" AND THE AUTHOR DISCLAIMS ALL WARRANTIES
%% WITH REGARD TO THIS SOFTWARE INCLUDING ALL IMPLIED WARRANTIES OF
%% MERCHANTABILITY AND FITNESS. IN NO EVENT SHALL THE AUTHOR BE LIABLE FOR
%% ANY SPECIAL, DIRECT, INDIRECT, OR CONSEQUENTIAL DAMAGES OR ANY DAMAGES
%% WHATSOEVER RESULTING FROM LOSS OF USE, DATA OR PROFITS, WHETHER IN AN
%% ACTION OF CONTRACT, NEGLIGENCE OR OTHER TORTIOUS ACTION, ARISING OUT OF
%% OR IN CONNECTION WITH THE USE OR PERFORMANCE OF THIS SOFTWARE.
%% 

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
