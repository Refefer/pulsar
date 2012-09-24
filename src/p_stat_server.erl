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

-module(p_stat_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, add_metrics/2, publish_metrics/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
    gen_server:start_link(?MODULE, [Args], []).

add_metrics(Pid, Metrics) ->
    gen_server:cast(Pid, {add_metrics, Metrics}).

publish_metrics(Pid) ->
    gen_server:call(Pid, {publish}).


%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-record(state, {table, site}).

%-spec init({atom(), binary()}) -> {atom(), pid()}
%      ; init({atom(), binary()}) -> {atom(), binary()}.

init([{site, Site}]) ->
    p_stat_utils:register_short_server(Site, self()),
    {ok, #state{table=ets:new(table, []), site=Site}}.

handle_call({publish}, _From, #state{table=Table} = State) ->
    Dict =dict:from_list(ets:tab2list(Table)),
    ets:delete_all_objects(Table),
    {reply, {ok, Dict}, State};

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add_metrics, Metrics}, #state{table=Table} = State) ->
    update_records(Table, Metrics),
    {noreply, State};

handle_cast(stop, State) ->
    {stop, normal, State};

handle_cast(Msg, State) ->
    io:format("Received ~p~n", [Msg]),
    {noreply, State}.

handle_info(Info, State) ->
    io:format("Uncaught Info: ~p~n", [Info]),
    {noreply, State}.

% Terminated due to error/crash of some sort.
terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
update_records(Table, []) ->
    ok;
update_records(Table, [{Metric, Value}|Rest]) ->
    Key = {Metric, Value},
    try ets:update_counter(Table, Key, {2, 1})
    catch
        error:_Reason ->
            ets:insert(Table, {Key, 1})
    end,
    update_records(Table, Rest).

get_date_time() ->
     {Date, Time} = erlang:localtime(),
     io_lib:format("~B-~B-~B ~B:~B:~B", 
          erlang:tuple_to_list(Date) ++ erlang:tuple_to_list(Time)).
