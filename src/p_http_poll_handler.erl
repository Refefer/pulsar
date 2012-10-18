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
-module(p_http_poll_handler).
-export([init/3, info/3, terminate/2]).

% Included so we can get the socket.
-include("deps/cowboy/include/http.hrl").

-record(state, {
    site :: binary(),
    metrics :: list(),
    lstat_server :: pid()
}).

init({tcp, http}, Req, [Opts]) ->
    erlang:send_after(0, self(), start),
    {loop, Req, Opts}.

info({reply, Body}, Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(200, [], Body, Req),
    {ok, Req2, State};

% We want to quit the second we get a message about the socket
% closing, so we have to set the socket to active.
info(start, Req=#http_req{socket=Socket}, Opts) ->
    {Site, Metrics, Req2} = p_http_utils:parse_request(Req, Opts),
    case pulsar_stat:add_long_metrics(Site, Metrics) of
        {ok, Server} ->
            % We want this process to die if the stat server dies
            inet:setopts(Socket, [{active, once}]),
            erlang:link(Server),
            {loop, Req2, #state{site=Site, metrics=Metrics, lstat_server=Server}, hibernate};
        {error, _Reason} ->
            {ok, FinalReq} = cowboy_http_req:reply(401, [], <<"">>, Req2),
            {ok, FinalReq, no_state}
    end;

% Client closed the connection, time to move along.
info({tcp_closed, _Port}, Req, State) ->
    {ok, Req, State};

% The bound server crashed for some reason, time to quit.
info({'EXIT', Server, _Reason}, Req, #state{lstat_server=Server}) ->
    {ok, Req, server_died};
    
% Probably should log rogue messages.
info(_Message, Req=#http_req{socket=Socket}, State) ->
    inet:setopts(Socket, [{active, once}]),
    {loop, Req, State, hibernate}.

terminate(_Req, server_died) ->
    ok;
terminate(_Req, undefined_state) ->
    ok;
terminate(_Req, #state{metrics=Metrics, lstat_server=Server}) ->
    % Add metric removal.
    p_lstat_server:remove_metrics(Server, Metrics),
    ok.
