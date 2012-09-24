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

-module(p_http_tick_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

% Included so we can cache qs, since that can be quite expensive.
-include("deps/cowboy/include/http.hrl").

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

handle(Req, State) ->
    {Host, Metrics, Req2} = p_http_utils:parse_request(Req),
    case pulsar_stat:add_short_metrics(Host, Metrics) of
        {error, not_defined} ->
            {ok, FinalReq} = cowboy_http_req:reply(401, [], <<"Site Not Watched">>, Req2);
        ok ->
            Headers = [{'Content-Type', <<"text/plain">>}],
            {ok, FinalReq} = cowboy_http_req:reply(200, Headers, Req2)
    end,
            
    {ok, FinalReq, State}.

terminate(_Req, _State) ->
    ok.
