-module(graph_rhythm_server).
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
            {[<<"tick">>, host], gr_http_tick_handler, []},
            {[<<"static">>, '...'], cowboy_http_static, 
                [{directory, <<"./priv/static">>},
                     {mimetypes, [
                        {<<".html">>, [<<"text/html">>]}
                     ]}
                ]},
            {[<<"watch">>, host], gr_http_watch_handler, []}
        ]}
    ],
    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    Pid = cowboy:start_listener(?SERVER, 100,
        cowboy_tcp_transport, [{port, 8080}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ),

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

