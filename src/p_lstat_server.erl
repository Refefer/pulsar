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
         add_metrics/2,
         remove_metrics/2,
         publish_metrics/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {site, dict}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).

add_metrics(Pid, Metrics) ->
    gen_server:cast(Pid, {add, Metrics}).

remove_metrics(Pid, Metrics) ->
    gen_server:cast(Pid, {remove, Metrics}).

publish_metrics(Pid) ->
    gen_server:call(Pid, {publish}).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([{site, Site}]) ->
    io:format("Starting ~p~n", [Site]),
    p_stat_utils:register_long_server(Site, self()),
    {ok, #state{site=Site, dict=dict:new()}}.

handle_call({publish}, _From, State=#state{dict=Dict}) ->
    {reply, {ok, Dict}, State#state{dict=Dict}};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add, Metrics}, State = #state{dict=Dict}) ->
    NewDict = add_records(Dict, Metrics),
    {noreply, State#state{dict=NewDict}};

handle_cast({remove, Metrics}, State = #state{dict=Dict}) ->
    NewDict = remove_records(Dict, Metrics),
    {noreply, State#state{dict=NewDict}};

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
% Adds a set of metrics to the dict
add_records(Dict, Metrics) ->
    lists:foldl(fun({Metric, Value}, AccDict) ->
        Key = {Metric, Value},
        dict:update_counter(Key, 1, AccDict)
    end, Dict, Metrics).

% Removes metrics from the dict, deleting ones that are
% zero or less.
remove_records(Dict, Metrics) ->
    lists:foldl(fun({Metric, Value}, AccDict) ->
        Key = {Metric, Value},
        Dict2 = dict:update_counter(Key, -1, AccDict),
        % Check if the key is now zero and delete it if it is
        case dict:fetch(Key, Dict2) of
            0 ->
                dict:erase(Key, Dict2);
            _Other ->
                Dict2
        end
    end, Dict, Metrics).
