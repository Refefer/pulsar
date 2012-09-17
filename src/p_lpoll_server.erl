-module(p_lpoll_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0, add_site/1, get_site/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

add_site(Site) ->
    case get_site_server(Site) of
        {ok, Pid} ->
            {ok, Pid};
        {error, not_defined} ->
            pg2:create({?MODULE, site, Site}),
            supervisor:start_child(p_lpoll_server_sup, [{site, Site}])
    end.

get_site(Site) ->
    get_site_server(Site).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    {ok, Args}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

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

