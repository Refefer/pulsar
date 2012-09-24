-module(pulsar_stat).
-export([add_host/1,
         remove_host/1,
         list_hosts/0,
         add_short_metrics/2,
         add_long_metrics/2,
         remove_long_metrics/2,
         publish_short_metrics/1]).

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

            % Really should only be one
            lists:foreach(fun(Pid) ->
                supervisor:terminate_child(p_stat_sup, Pid)
            end, Sups)
    end.
            
add_long_metrics(Host, Metrics) ->
    case get_long_server(Host) of 
        {ok, Pid} ->
            p_lstat_server:add_metrics(Pid, Metrics);
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

% Talks to all short servers, merging their dictionaries
% together.
publish_short_metrics(Host) ->
    merge_metrics(lists:map(fun(Server) ->
        {ok, Dict} = p_stat_server:publish_metrics(Server),
        Dict
    end, get_short_servers(Host))).
    
list_hosts() ->
    Hosts = lists:foldl(fun(Group, Hosts) ->
        case Group of
            {?MODULE, super, Host} ->
                [Host|Hosts];
            Group ->
                Hosts
        end
    end, [], pg2:which_groups()).

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------

get_short_server(Host) ->
    get_server(p_stat_utils:get_short_stat_group(Host)).

get_short_servers(Host) ->
    pg2:get_members(p_stat_utils:get_short_stat_group(Host)).

get_long_server(Host) ->
    get_server(p_stat_utils:get_long_stat_group(Host)).

get_server(Group) ->
    case pg2:get_closest_pid(Group) of
        Pid ->
            {ok, Pid};
        {error, {no_such_group, Group}} ->
            {error, not_defined}
    end.

% Merges all dictionaries together, adding their respective values
% together.
merge_metrics([Dict|Rest]) ->
    lists:foldl(fun(Dict1, Dict2) ->
        dict:merge(fun(_K, V1, V2) ->
            V1+V2
        end, Dict1, Dict2)
    end, Dict, Rest).
