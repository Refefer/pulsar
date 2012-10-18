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

-module(p_http_utils).
-export([get_qs/1,
        parse_request/1,
        parse_request/2]).

% Included so we can cache qs, since that can be quite expensive.
-include("deps/cowboy/include/http.hrl").
-include("pulsar.hrl").

% Gets the query values from the path, looking up and caching them.
get_qs(#http_req{raw_qs=RawQs} = Req) ->
    case simple_cache:lookup({qs, RawQs}) of
        {error, missing} ->
            {Qs, Req2} = cowboy_http_req:qs_vals(Req),
            simple_cache:set({qs, RawQs}, {Qs, Req2#http_req.qs_vals}, 60),
            {Qs, Req2};

        {ok, {Qs, AllQs}} ->
            Req2 = Req#http_req{qs_vals=AllQs},
            {Qs, Req2}
    end.

% Individual mapping of one stat to another
cross_tab_metrics({RKey1, RKey2}, Metrics) ->
    Key1 = erlang:list_to_binary(RKey1),
    Key2 = erlang:list_to_binary(RKey2),
    case lists:keytake(Key1, 1, Metrics) of
        false -> [];
        {value, {Key1, Value1}, Rest} ->
            case lists:keysearch(Key2, 1, Rest) of
                false -> [];
                {value, {Key2, Value2}} -> [{{Key1, Value1, Key2}, Value2}]
            end
    end;
                    
% Maps all metrics for one Key to all others
cross_tab_metrics(RKey, Metrics) ->
    Key = erlang:list_to_binary(RKey),
    case lists:keytake(Key, 1, Metrics) of
        false ->
            [];
        {value, {Key, Value}, Rest} ->
            StatsBefore = lists:map(fun({SubKey, V}) ->
                {{Key, Value, SubKey}, V}
            end, Rest),
            StatsAfter = lists:map(fun({NewKey, V}) ->
                {{NewKey, V, Key}, Value}
            end, Rest),
            lists:flatten([StatsBefore, StatsAfter])
    end.

% Maps all the headers to a set of functions
map_headers(Headers, Req) ->
    Result = lists:foldl(fun({Header, Module, Callback, Opts}, Stats) ->
        {Val, Req2} = cowboy_http_req:header(Header, Req, missing),
        [Module:Callback({Header, Val}, Opts) | Stats]
    end, [], Headers),
    lists:flatten(Result).

% Parses out the useful information from a Request object.
parse_request(Req) ->
    parse_request(Req, []).

parse_request(Req, Options) ->
    HeaderStats = map_headers(Options#field_opts.headers, Req),
    {QS, Req2} = get_qs(Req),
    MostMetrics = lists:flatten([HeaderStats, QS]),

    % Figure out which ones are crosstabbed and cross tab them.
    Metrics = case Options#field_opts.crosstab_fields of
        [] ->
            MostMetrics;
        Keys ->
            CrossTabLists = lists:map(fun(Key) ->
                cross_tab_metrics(Key, MostMetrics)
            end, Keys),
            lists:flatten([MostMetrics|CrossTabLists])
    end,

    AllMetrics = [{<<"stats">>, <<"total">>} | Metrics],
    {Host, Req3} = cowboy_http_req:binding(host, Req2),
    {Host, AllMetrics, Req3}.
