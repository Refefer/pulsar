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

-module(p_history_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).
-include("pulsar.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, subscribe/2, get_key/3]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

% To be fleshed out with more features, such as persistance
-record(state, {host,
                tref,
                max_items,
                listeners=[],
                tables=gb_trees:empty()}).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
    gen_server:start_link(?MODULE, Args, []).

subscribe(ServerPid, SubscriberPid) ->
    gen_server:cast(ServerPid, {subscribe, SubscriberPid}).

get_key(ServerPid, Timestamp, Key) ->
    case get_table(ServerPid, Timestamp) of
        {ok, Table} ->
            Dict = dict:from_list(lists:map(fun([K,V]) ->
                {K, V}
            end, ets:match(Table, {{Key, '$1'}, '$2'}))),
            Dict;
        {error, not_found} ->
            dict:new()
    end.

get_table(ServerPid, Timestamp) ->
    gen_server:call(ServerPid, {get_table, Timestamp}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Props) ->
    io:format("Properties: ~p~n", [Props]),
    Site = proplists:get_value(site, Props),
    p_stat_utils:register_history_server(Site, self()),
    MaxItems =  proplists:get_value(max_memory_items, Props, 6),
    TRef = timer:send_interval(?TICK_INTERVAL, tick),
    {ok, #state{host=Site,
                tref=TRef,
                max_items=MaxItems}}.

handle_call({get_table, Timestamp}, _From, #state{tables=TableIdx} = State) ->
    {reply, lookup_table(TableIdx, Timestamp), State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

% Adds a listener to the history server.
handle_cast({subscribe, Pid}, #state{listeners=Listeners} = State) ->
    {noreply, State#state{listeners=[Pid|Listeners]}};

% Handle save
handle_cast(_Msg, State) ->
    {noreply, State}.

% Time to collect our incremental statistics.
handle_info(tick, #state{host=Host, listeners=Listeners, tables=TablesIdx, max_items=MaxItems} = State) ->
    Time = erlang:localtime(),
    Dict = pulsar_stat:publish_all_metrics(Host),
    Table = p_history_utils:dict_to_ets(Dict),

    % Let people know we exist
    CurListeners = broadcast_published(Host, Time, Listeners),

    % Store the table
    pulsar_stat:store_table(Host, Time, Table),

    % Remove old tables
    CleanTables = clean_tables(MaxItems, TablesIdx),

    % Add new table
    NewTablesIdx = gb_trees:insert(Time, Table, CleanTables),
        
    {noreply, State#state{listeners=CurListeners, tables=NewTablesIdx}};

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
broadcast_published(Host, Time, Listeners) ->
    lists:foldl(fun(Pid, AlivePids) ->
        case erlang:is_process_alive(Pid) of
            true ->
                Pid ! {published, self(), Time, Host},
                [Pid | AlivePids];
            false ->
                AlivePids
        end
    end, [], Listeners).
            
% Clean out the oldest tables  when we exceed the Max Items.
clean_tables(MaxItems, TablesIdx) ->
    Size = gb_trees:size(TablesIdx),
    if
        Size > MaxItems ->
            {_K, Table, CleanTables} = gb_trees:take_smallest(TablesIdx),
            ets:delete(Table),
            CleanTables;
        true ->
            TablesIdx
    end.

lookup_table(Tables, Timestamp) ->
    case gb_trees:lookup(Timestamp, Tables) of
        none ->
            {error, not_found};
        {value, Table} ->
            {ok, Table}
    end.
