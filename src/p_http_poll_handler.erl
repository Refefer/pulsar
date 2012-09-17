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

% Included so we can cache qs, since that can be quite expensive.
-include("deps/cowboy/include/http.hrl").

-record(state, {
    site :: binary(),
    metrics :: list(),
    link_server :: pid()
}).

init({tcp, http}, Req, _Opts) ->
    io:format("Starting~n", []),
    erlang:send_after(0, self(), start),
    {loop, Req, undefined_state}.

info({reply, Body}, Req, State) ->
    {ok, Req2} = cowboy_http_req:reply(200, [], Body, Req),
    {ok, Req2, State};

% We want to quite the second we get a message about the socket
% closing, so we have to set the socket to active.
info(start, Req=#http_req{socket=Socket}, State) ->
    inet:setopts(Socket, [{active, true}]),
    {Site, Metrics, Req2} = p_http_utils:parse_request(Req),
    case p_link_server:get_site(Site) of
        {ok, Server} ->
            io:format("Adding metrics~n", []),
            % We want this process to die if the server is dead.
            erlang:link(Server),
            {loop, Req2, #state{site=Site, metrics=Metrics, link_server=Server}, hibernate};
        {error, not_defined} ->
            {ok, FinalReq} = cowboy_http_req:reply(401, [], <<"">>, Req2),
            {ok, FinalReq, State}
    end;
% Immediately close.
info({tcp_closed, _Port}, Req, State) ->
    {ok, Req, State};

% Probably should rogue messages.
info(_Message, Req, State) ->
    {loop, Req, State, hibernate}.

terminate(Req, undefined_state) ->
    ok;
terminate(Req, #state{site=Site, metrics=Metrics, link_server=Server}) ->
    io:format("Removing metrics~n", []),
    % Add metric removal.
    ok.
