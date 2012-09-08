-module(pulsar_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/0]).

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

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init(Args) ->
    Dispatch = [
        %% {Host, list({Path, Handler, Opts})}
        {'_', [
            {[<<"tick">>, host], p_http_tick_handler, []},
            {[<<"static">>, '...'], cowboy_http_static, 
                [{directory, <<"./priv/static">>},
                     {mimetypes, [
                        {<<".html">>, [<<"text/html">>]},
                        {<<".js">>, [<<"application/javascript">>]}
                     ]}
                ]},
            {[<<"site">>, command], p_http_site_handler, []}
        ]}
    ],
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    Pid = cowboy:start_listener(?SERVER, 150,
        cowboy_tcp_transport, [{port, 8080}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ),

    % Load static sites, if available
    case application:get_env(pulsar, hosts) of
        undefined ->
            ok;
        {ok, Hosts} ->
            lists:foreach(fun(Site) ->
                p_stat_server:add_site(erlang:atom_to_binary(Site, latin1))
            end, Hosts)
    end,

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

