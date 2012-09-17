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

-module(p_lstat_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1,
        add_site/1,
        get_site/1,
        add_metrics/2,
        remove_metrics/2,
        get_key/2]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {site, table}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Args], []).

add_site(Site) ->
    case get_site_server(Site) of
        {ok, Pid} ->
            {ok, Pid};
        {error, not_defined} ->
            pg2:create({?MODULE, site, Site}),
            supervisor:start_child(p_lstat_server_sup, [{site, Site}])
    end.

get_site(Site) ->
    get_site_server(Site).

add_metrics(Site, Metrics) ->
    gen_server:cast(Site, {add, Metrics}).

remove_metrics(Site, Metrics) ->
    gen_server:cast(Site, {remove, Metrics}).

get_key(Site, Key) ->
    Table = gen_server:call(Site, {get_table}),
    dict:from_list(lists:map(fun([K,V]) ->
        {K, V}
    end, ets:match(Table, {{Key, '$1'}, '$2'}))).
%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([{site, Site}]) ->
    register_site(Site, self()),
    Table = ets:new(?MODULE, [{read_concurrency, true}]),
    {ok, #state{site=Site,table=Table}}.

handle_call({get_table}, _From, State =#state{table=Table}) ->
    {reply, Table, State};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add, Metrics}, State = #state{table=Table}) ->
    add_records(Table, Metrics),
    {noreply, State};

handle_cast({remove, Metrics}, State = #state{table=Table}) ->
    remove_records(Table, Metrics),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
get_site_server(Name) ->
    case pg2:get_members({?MODULE, site, Name}) of
        [Pid] ->
            {ok, Pid};
        [] ->
            {error, died};
        {error, _Reason} ->
            {error, not_defined}
    end.

register_site(Site, Pid) ->
    pg2:join({?MODULE, site, Site}, Pid).

% Adds a set of metrics to the table
add_records(Table, Metrics) ->
    lists:foreach(fun({Metric, Value}) ->
        Key = {Metric, Value},
        try ets:update_counter(Table, Key, {2, 1})
        catch
            error:_Reason ->
                ets:insert(Table, {Key, 1})
        end
    end, Metrics).

% Removes metrics from the table, deleting ones that are
% zero or less.
remove_records(Table, Metrics) ->
    lists:foreach(fun({Metric, Value}) ->
        Key = {Metric, Value},
        try Value = ets:update_counter(Table, Key, {2, -1})
        catch
            error:_Reason ->
                Value = undefined
        end,
        case Value of 
            0 ->
                ets:delete(Table, Key);
            _Other ->
                ok
        end
    end, Metrics).

