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
    case application:get_env(pulsar, static_dir) of
        undefined ->
            Static = <<"./priv/static">>;
        {ok, RawStatic} ->
            Static = erlang:list_to_binary(RawStatic)
    end,

    Dispatch = [
        %% {Host, list({Path, Handler, Opts})}
        {'_', [
            {[<<"tick">>, host], p_http_tick_handler, []},
            {[<<"poll">>, host], p_http_poll_handler, []},
            {[<<"static">>, '...'], cowboy_http_static, 
                [{directory, Static},
                     {mimetypes, [
                        {<<".html">>, [<<"text/html">>]},
                        {<<".js">>, [<<"application/javascript">>]}
                     ]}
                ]},
            {[<<"site">>, command], p_http_site_handler, []}
        ]}
    ],

    case application:get_env(pulsar, port) of
        undefined ->
            Port = 8080;
        {ok, Port} ->
            Port
    end,

    % get the max connections
    case application:get_env(pulsar, max_connections) of
        undefined ->
            MaxConns = 50000;
        {ok, MaxConns} ->
            ok
    end,

    %% Name, NbAcceptors, Transport, TransOpts, Protocol, ProtoOpts
    _Pid = cowboy:start_listener(?SERVER, 150,
        cowboy_tcp_transport, [{port, Port}, {max_connections, MaxConns}],
        cowboy_http_protocol, [{dispatch, Dispatch}]
    ),

    % Load static sites, if available
    case application:get_env(pulsar, hosts) of
        undefined ->
            ok;
        {ok, Hosts} ->
            lists:foreach(fun(Site) ->
                p_stat_server:add_site(list_to_binary(Site)),
                p_lstat_server:add_site(list_to_binary(Site))
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

