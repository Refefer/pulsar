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

-export([start_link/1,
        subscribe/2,
        get_key/2,
        get_key/3,
        query_between/4]).

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

% Get the key from the most recent table
get_key(ServerPid, Key) ->
    case get_latest_table(ServerPid) of
        {ok, {Timestamp, Table}} ->
            Results = query_key_in_table(Key, Table),
            {Timestamp, Results};
        {error, not_found} ->
            {erlang:localtime(), []}
    end.

get_key(ServerPid, Timestamp, Key) ->
    {ok, Tree} = query_between(ServerPid, Key, Timestamp, Timestamp),
    case gb_trees:is_empty(Tree) of
        false ->
            Tree;
        true ->
            gb_tree:insert(Timestamp, [], Tree)
    end.

query_between(ServerPid, Key, Start, End) ->
    Tables = gen_server:call(ServerPid, {query_between, Start, End}),
    Results = lists:foldl(fun({Tmsp, Table}, Tree) ->
        gb_trees:insert(Tmsp, query_key_in_table(Key, Table), Tree)
    end, gb_trees:empty(), gb_trees:to_list(Tables)),
    {ok, Results}.

get_latest_table(ServerPid) ->
    gen_server:call(ServerPid, {get_latest_table}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Props) ->
    Site = proplists:get_value(site, Props),
    p_stat_utils:register_history_server(Site, self()),
    MaxItems =  proplists:get_value(max_memory_items, Props, 6),
    TRef = timer:send_interval(?TICK_INTERVAL, tick),
    {ok, #state{host=Site,
                tref=TRef,
                max_items=MaxItems}}.

handle_call({query_between, Start, End}, _From, #state{tables=TableIdx} = State) ->
    {reply, tables_between(TableIdx, Start, End), State};

handle_call({get_latest_table}, _From, #state{tables=TableIdx} = State) ->
    Response = case gb_trees:is_empty(TableIdx) of
        true ->
            {error, not_found};
        false ->
            {Timestamp, Table} = gb_trees:largest(TableIdx),
            {ok, {Timestamp, Table}}
    end,
    {reply, Response, State};

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

tables_between(Tables, Start, End) ->
    lists:foldl(fun({Tmsp, Table}, Tree) ->
        if 
            (Tmsp >= Start) andalso (Tmsp =< End) ->
                gb_trees:insert(Tmsp, Table, Tree);
            true ->
                Tree
        end
    end, gb_trees:empty(), gb_trees:to_list(Tables)).

query_key_in_table(Key, Table) ->
    lists:map(fun([K,V]) ->
        {K, V}
    end, ets:match(Table, {{Key, '$1'}, '$2'})).

