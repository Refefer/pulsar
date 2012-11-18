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

-module(p_stat_sup).
-behaviour(supervisor).

%% API
-export([start_link/0, start_link/1, shutdown_server/1]).

%% Supervisor callbacks
-export([init/1]).

%% Helper macro for declaring children of supervisor
-define(CHILD(I, Type, Args), {I, {I, start_link, Args}, permanent, 5000, Type, [I]}).

%% ===================================================================
%% API functions
%% ===================================================================

start_link() ->
    start_link([]).

start_link(Args) ->
    case supervisor:start_link(?MODULE, [Args]) of
        {ok, Pid} -> 
            {ok, Pid};
        {error, {already_started, Pid}} ->
            {already_started, Pid}
    end.

shutdown_server(Pid) ->
    supervisor:terminate_child(?MODULE, Pid).

%% ===================================================================
%% Supervisor callbacks
%% ===================================================================

init([{site, Site}]) ->
    StatServer = ?CHILD(p_stat_server_sup, supervisor, [{site, Site}]),
    LStatServer = ?CHILD(p_lstat_server_sup, supervisor, [{site, Site}]),
    HistoryServer = ?CHILD(p_history_server_sup, supervisor, [[{site, Site}]]),
    PersisterServer = case application:get_env(pulsar, persister) of
        undefined ->
            ?CHILD(p_persister_null, worker , [[{site, Site}]]);
        {ok, {Persister, Props}} ->
            ?CHILD(Persister, worker, [[{site, Site}|Props]])
    end,
    {ok, { { one_for_one, 5, 10}, [StatServer, LStatServer, HistoryServer, PersisterServer]} }.

