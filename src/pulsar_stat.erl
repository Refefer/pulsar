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

-module(pulsar_stat).
-export([add_host/1,
         remove_host/1,
         list_hosts/0,
         add_short_metrics/2,
         add_long_metrics/2,
         store_table/3,
         remove_long_metrics/2,
         publish_short_metrics/1,
         publish_long_metrics/1,
         publish_all_metrics/1,
         query_host/2,
         query_host/3,
         subscribe/1,
         subscribe/2]).

-define(SUPER(Host), {?MODULE, super, Host}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

add_host(Host) ->
    case lists:member(?SUPER(Host), pg2:which_groups()) of
        false ->
            pg2:create(?SUPER(Host)),
            pg2:create(p_stat_utils:get_short_stat_group(Host)),
            pg2:create(p_stat_utils:get_long_stat_group(Host)),
            pg2:create(p_stat_utils:get_history_group(Host)),
            pg2:create(p_stat_utils:get_persister_group(Host)),
            {ok, Pid} = supervisor:start_child(pulsar_stat_sup,[{site, Host}]),
            pg2:join(?SUPER(Host), Pid);
        true ->
            % Already added, move along.
            ok
   end.

remove_host(Host) ->
    case lists:get_members(?SUPER(Host), pg2:which_groups()) of
        {error, {no_such_group, _Group}} ->
            % Doesn't exist, must have been removed.
            ok;
        Sups ->
            pg2:delete(?SUPER(Host)),
            pg2:delete(p_stat_utils:get_short_stat_group(Host)),
            pg2:delete(p_stat_utils:get_long_stat_group(Host)),
            pg2:delete(p_stat_utils:get_history_group(Host)),
            pg2:delete(p_stat_utils:get_persister_group(Host)),

            % Really should only be one
            lists:foreach(fun(Pid) ->
                supervisor:terminate_child(p_stat_sup, Pid)
            end, Sups)
    end.
            
add_long_metrics(Host, Metrics) ->
    case get_long_server(Host) of 
        {ok, Pid} ->
            p_lstat_server:add_metrics(Pid, Metrics),
            {ok, Pid};
        {error, Reason} ->
            {error, Reason}
    end.

remove_long_metrics(Host, Metrics) ->
    case get_long_server(Host) of 
        {ok, Pid} ->
            p_lstat_server:remove_metrics(Pid, Metrics);
        {error, Reason} ->
            {error, Reason}
    end.

add_short_metrics(Host, Metrics) ->
    case get_short_server(Host) of 
        {ok, Pid} ->
            p_stat_server:add_metrics(Pid, Metrics);
        {error, Reason} ->
            {error, Reason}
    end.

store_table(Host, Time, Table) ->
    case get_persister_server(Host) of
        {ok, Pid} ->
            Module = case application:get_env(pulsar, persister) of
                undefined ->
                    p_persister_null;
                {ok, {P, _Props}} ->
                    P
            end,
            Module:store_table(Pid, Host, Time, Table);
        {error, Reason} ->
            {error, Reason}
    end.
            

% Talks to all short servers, merging their dictionaries
% together.
publish_short_metrics(Host) ->
    merge_metrics(lists:map(fun(Server) ->
        {ok, Dict} = p_stat_server:publish_metrics(Server),
        Dict
    end, get_short_servers(Host))).

publish_long_metrics(Host) ->
    merge_metrics(lists:map(fun(Server) ->
        {ok, Dict} = p_lstat_server:publish_metrics(Server),
        Dict
    end, get_long_servers(Host))).
    
publish_all_metrics(Host) ->
    merge_metrics([publish_long_metrics(Host), publish_short_metrics(Host)]).

% Lists all active hosts.
list_hosts() ->
    lists:foldl(fun(Group, Hosts) ->
        case Group of
            {?MODULE, super, Host} ->
                [Host|Hosts];
            Group ->
                Hosts
        end
    end, [], pg2:which_groups()).

subscribe(Host) ->
    subscribe(Host, self()).

subscribe(Host, Pid) ->
    case get_history_server(Host) of
        {ok, SPid} ->
            p_history_server:subscribe(SPid, Pid);
        {error, Reason} ->
            {error, Reason}
    end.

query_host(Host, Timestamp, Key) ->
    case get_history_server(Host) of
        {ok, SPid} ->
            p_history_server:get_key(SPid, Timestamp, Key);
        {error, Reason} ->
            {error, Reason}
    end.
query_host(Host, Key) ->
    case get_history_server(Host) of
        {ok, SPid} ->
            p_history_server:get_key(SPid, Key);
        {error, Reason} ->
            {error, Reason}
    end.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_short_server(Host) ->
    get_server(p_stat_utils:get_short_stat_group(Host)).

get_short_servers(Host) ->
    pg2:get_members(p_stat_utils:get_short_stat_group(Host)).

get_long_server(Host) ->
    get_server(p_stat_utils:get_long_stat_group(Host)).

get_long_servers(Host) ->
    pg2:get_members(p_stat_utils:get_long_stat_group(Host)).

get_history_server(Host) ->
    get_server(p_stat_utils:get_history_group(Host)).

get_persister_server(Host) ->
    get_server(p_stat_utils:get_persister_group(Host)).

get_server(Group) ->
    case pg2:get_closest_pid(Group) of
        {error, {no_such_group, Group}} ->
            {error, not_defined};
        Pid ->
            {ok, Pid}
    end.

% Merges all dictionaries together, adding their respective values
% together.
merge_metrics([Dict|Rest]) ->
    lists:foldl(fun(Dict1, Dict2) ->
        dict:merge(fun(_K, V1, V2) ->
            V1+V2
        end, Dict1, Dict2)
    end, Dict, Rest).
