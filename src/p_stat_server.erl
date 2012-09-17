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
-include("shared.hrl").

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, add_site/1, add_metrics/2, 
         add_site_listener/2, remove_site_listener/2,
         list_sites/0, delete_site/1]).

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

add_site(Site) ->
    case get_site(Site) of
        {ok, Pid} ->
            {ok, Pid};
        {error, not_defined} ->
            pg2:create({?MODULE, site, Site}),
            pg2:create({?MODULE, listener, Site}),
            supervisor:start_child(p_stat_server_sup, [{site, Site}])
    end.

delete_site(Site) ->
    using_site(Site, fun(Pid) -> 
        gen_server:cast(Pid, stop)
    end).

list_sites() ->
    Sites = lists:filter(fun(Group) -> 
        case Group of
            {?MODULE, site, _Site} ->
                true;
            Group ->
                false
        end
    end, pg2:which_groups()),
    lists:map(fun({_Module, site, Site}) -> Site end, Sites).

add_site_listener(Site, Pid) ->
    using_site(Site, fun(_Pid) ->
        register_listener(Site, Pid)
    end).

remove_site_listener(Site, Pid) ->
    unregister_listener(Site, Pid).

add_metrics(Site, Metrics) ->
    using_site(Site, fun(Pid) -> 
        gen_server:cast(Pid, {add_metrics, Metrics})
    end).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-record(state, {site, table, tref}).

%-spec init({atom(), binary()}) -> {atom(), pid()}
%      ; init({atom(), binary()}) -> {atom(), binary()}.

init([{site, Site}]) ->
    register_site(Site, self()),
    % Load up the pinger
    {ok, TRef} = timer:send_interval(?TICK_INTERVAL, {event, ping}),
    {ok, #state{site=Site, table=new_ets(), tref=TRef}}.

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

% Times up!  Send the list to the handlers
handle_info({event, ping}, State) ->
    send_urls(State), 
    {noreply, State#state{table=new_ets()}};

handle_info(Info, State) ->
    io:format("Uncaught Info: ~p~n", [Info]),
    {noreply, State}.

terminate(Reason, #state{site=Site, tref=Tref}) ->
    % Cleanup listeners
    send_terminate_message(Site),

    % Stop the timer
    timer:cancel(Tref),

    % Clean up groups.
    pg2:delete({?MODULE, site, Site}),
    pg2:delete({?MODULE, listener, Site}),
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
send_terminate_message(Site) ->
    case get_listeners(Site) of
        [] ->
            ok;
        Members ->
            lists:foreach(fun(Pid) ->
                Pid ! {?MODULE, {terminate, Site}} end, Members)
    end.

send_urls(#state{site=Site, table=Table} ) ->
    Pid = spawn(fun() ->
        case get_listeners(Site) of
            [] ->
                % No listeners, nothing to do
                ok;
            Members ->
                {Date, Time} = erlang:localtime(),
                DateTime = io_lib:format("~B-~B-~B ~B:~B:~B", erlang:tuple_to_list(Date) ++ erlang:tuple_to_list(Time)),
                lists:foreach(fun(Pid) ->
                    Pid ! {?MODULE, {stats, DateTime, Table}}
                end, Members),
                % Sleep one minute before cleaning up.
                timer:sleep(60000)
        end
    end),
    % We give away the table to make sure that it properly gets
    % cleaned up.  TODO: Make this a smarter gen_fsm.
    ets:give_away(Table, Pid, []).

get_site(Name) ->
    case pg2:get_members({?MODULE, site, Name}) of
        [Pid] ->
            {ok, Pid};
        [] ->
            {error, died};
        {error, _Reason} ->
            {error, not_defined}
    end.

using_site(Site, Fun) ->
    case get_site(Site) of
        {ok, Pid} ->
            Fun(Pid);
        {error, not_defined} ->
            case application:get_env(pulsar, dynamic_host) of
                {ok, true} ->
                    add_site(Site),
                    using_site(Site, Fun);
                _Other ->
                    {error, not_defined}
            end
    end.

update_records(Table, Metrics) ->
    lists:foreach(fun({Metric, Value}) ->
        Key = {Metric, Value},
        try ets:update_counter(Table, Key, {2, 1})
        catch
            error:_Reason ->
                ets:insert(Table, {Key, 1})
        end
    end, Metrics).

new_ets() ->
    ets:new(?MODULE, []).

get_listeners(Site) ->
    pg2:get_members({?MODULE, listener, Site}).

register_listener(Site, Pid) ->
    pg2:join({?MODULE, listener, Site}, Pid).

unregister_listener(Site, Pid) ->
    pg2:leave({?MODULE, listener, Site}, Pid).

register_site(Site, Pid) ->
    pg2:join({?MODULE, site, Site}, Pid).

