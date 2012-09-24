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
-include("shared.hrl").

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
                listeners=[],
                timestamp=nil,
                current_table=nil}).
%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).

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

init([{site, Site}]) ->
    p_stat_utils:register_history_server(Site, self()),
    TRef = timer:send_interval(?TICK_INTERVAL, tick),
    {ok, #state{host=Site, tref=TRef}}.

handle_call({get_table, Timestamp}, _From, #state{timestamp=Timestamp, current_table=Table} = State) ->
    {reply, {ok, Table}, State};
handle_call({get_table, _Timestamp}, _From, State) ->
    {reply, {error, not_found}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

% Adds a listener to the history server.
handle_cast({subscribe, Pid}, #state{listeners=Listeners} = State) ->
    {noreply, State#state{listeners=[Pid|Listeners]}};
handle_cast(_Msg, State) ->
    {noreply, State}.

% Time to collect our incremental statistics.
handle_info(tick, #state{host=Host, listeners=Listeners} = State) ->
    Time = erlang:localtime(),
    Dict = pulsar_stat:publish_short_metrics(Host),
    Table = p_history_utils:dict_to_ets(Dict),
    NewListeners = broadcast_published(Host, Time, Listeners),
    {noreply, State#state{timestamp=Time, current_table=Table, listeners=NewListeners}};

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
            
time_to_string({{Year, Month, Day},{Hour, Minute, Second}}) ->
    io_lib:format("~s-~s-~s ~s:~s:~s", [Year, Month, Day, Hour, Minute, Second]).
