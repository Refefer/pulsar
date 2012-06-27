-module(gr_site_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, add_site/1, add_url/2, 
         add_site_listener/2, remove_site_listener/2]).

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
            supervisor:start_child(gr_site_server_sup, [{site, Site}])
    end.

add_site_listener(Site, Pid) ->
    register_listener(Site, Pid).

remove_site_listener(Site, Pid) ->
    unregister_listener(Site, Pid).

add_url(Site, {Url, Ref}) ->
    using_site(Site, fun(Pid) -> 
        gen_server:cast(Pid, {add_url, {Url, Ref}})
    end).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------
-record(state, {site, urls=[]}).

%-spec init({atom(), binary()}) -> {atom(), pid()}
%      ; init({atom(), binary()}) -> {atom(), binary()}.

init([{site, Site}]) ->
    register_site(Site, self()),
    % Load up the pinger
    timer:send_interval(1000, {event, ping}),
    {ok, #state{site=Site}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast({add_url, Url}, #state{urls=Urls} = State) ->
    {noreply, State#state{urls=[Url|Urls]}};

handle_cast(Msg, State) ->
    io:format("Received ~p~n", [Msg]),
    {noreply, State}.

% Times up!  Send the list to the handlers
handle_info({event, ping}, #state{site=Site} = State) ->
    send_urls(State), 
    {noreply, #state{site=Site}};
handle_info(Info, State) ->
    io:format("Uncaught Info: ~p~n", [Info]),
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
send_urls(#state{site=Site, urls=Urls} ) ->
    spawn(fun() ->
        case get_listeners(Site) of
            [] ->
                % No listeners, nothing to do
                ok;
            Members ->
                SortedUrls = lists:sort(Urls),
                LocalTime = erlang:localtime(),
                lists:foreach(fun(Pid) ->
                    Pid ! {urls, LocalTime, SortedUrls}
                end, Members)
        end
    end).

get_site(Name) ->
    case pg2:get_members({?MODULE, site, Name}) of
        [Pid] ->
            {ok, Pid};
        {error, _Reason} ->
            {error, not_defined}
    end.

using_site(Site, Fun) ->
    case get_site(Site) of
        {ok, Pid} ->
            Fun(Pid);
        Other ->
            Other
    end.

get_listeners(Site) ->
    pg2:get_members({?MODULE, listener, Site}).

register_listener(Site, Pid) ->
    pg2:join({?MODULE, listener, Site}, Pid).

unregister_listener(Site, Pid) ->
    pg2:leave({?MODULE, listener, Site}, Pid).

register_site(Site, Pid) ->
    pg2:join({?MODULE, site, Site}, Pid).

