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
         get_query_key/1,
         apply_query_filters/2,
         parse_request/1,
         parse_request/2,
         time_to_string/1,
         string_to_time/1,
         ret_json/3]).

% Included so we can cache qs, since that can be quite expensive.
-include("deps/cowboy/include/http.hrl").
-include("pulsar.hrl").

% Gets the query values from the path, looking up and caching them.
get_qs(Req) ->
    cowboy_http_req:qs_vals(Req).

with_key(Key, Metrics, Fun) ->
    case lists:keytake(Key, 1, Metrics) of
        false -> [];
        {value, {Key, Value}, Rest} ->
            Fun(Value, Rest)
    end.

% Allow a metric to drill in  
cross_tab_metrics({Key, all}, Metrics) ->
    BKey = erlang:list_to_binary(Key),
    with_key(BKey, Metrics, fun(Value, Rest) ->
        lists:map(fun({SubKey, V}) ->
            {{BKey, Value, SubKey}, V}
        end, Rest)
    end);

% Allow a metric as a second index
cross_tab_metrics({all, Key}, Metrics) ->
    BKey = erlang:list_to_binary(Key),
    with_key(BKey, Metrics, fun(Value, Rest) ->
        lists:map(fun({SubKey, V}) ->
            {{SubKey, V, BKey}, Value}
        end, Rest)
    end);

% Individual mapping of one stat to another
cross_tab_metrics({RKey1, RKey2}, Metrics) ->
    Key1 = erlang:list_to_binary(RKey1),
    Key2 = erlang:list_to_binary(RKey2),
    with_key(Key1, Metrics, fun(Value1, Rest) ->
        case lists:keysearch(Key2, 1, Rest) of
            false -> [];
            {value, {Key2, Value2}} -> [{{Key1, Value1, Key2}, Value2}]
        end
    end);
                    
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
        {Val, _Req} = cowboy_http_req:header(Header, Req, missing),
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
            lists:flatten([MostMetrics, CrossTabLists])
    end,
    AllMetrics = [{<<"stats">>, <<"total">>} | Metrics],
    {Host, Req3} = cowboy_http_req:binding(host, Req2),
    {Host, AllMetrics, Req3}.

% Returns the query from a request object
get_query_key(Req) ->
    {Key, Req2} = cowboy_http_req:binding(key, Req),
    {Qs, Req3} = p_http_utils:get_qs(Req2),
    Results = lists:filter(fun({QKey, _Value}) -> 
        case QKey of 
            << "_", _Rest/binary>> ->
                false;
            QKey ->
                true
        end
    end, Qs),
    case Results of
        [{FilterKey, FilterValue}|_Rest] ->
            {{FilterKey, FilterValue, Key}, Req3};
        _Other ->
            {Key, Req3}
    end.

% Filters data based on arguments passed in, respecting order.
apply_query_filters(Values, Req) ->
    {Qs, Req2} = p_http_utils:get_qs(Req),
    SortedValues = lists:sort(fun({_K1, V}, {_K2, V2}) ->
        V >= V2
    end, Values),
    apply_filters(Qs, SortedValues, Req2).

apply_filters([], Values, _Req) ->
    Values;
% Takes the first Amount items from the list
apply_filters([{<<"_limit">>, Amount} | Rest], Values, Req) ->
    Num = list_to_integer(binary_to_list(Amount)),
    apply_filters(Rest, lists:sublist(Values, Num), Req);
% Reverses the list
apply_filters([{<<"_reverse">>, _Dir} | Rest], Values, Req) ->
    apply_filters(Rest, lists:reverse(Values), Req);
% Grabs only the provided value
apply_filters([{<<"_value">>, DKey} | _Rest], Values, _Req) ->
    lists:filter(fun({Key, _V}) ->
        Key == DKey
    end, Values);
% Minimum value to return
apply_filters([{<<"_min">>, Amount} | _Rest], Values, _Req) ->
    Num = list_to_integer(binary_to_list(Amount)),
    lists:filter(fun({_Key, Value}) ->
        Value >= Num
    end, Values);
apply_filters([_Unknown | Rest], Values, Req) ->
    apply_filters(Rest, Values, Req).

% Converts an erlang time to string
time_to_string({{Year, Month, Day},{Hour, Minute, Second}}) ->
    io_lib:format("~p-~p-~p_~p:~p:~p", [Year, Month, Day, Hour, Minute, Second]).
string_to_time(String) ->
    {ok, [Year, Month, Day, Hour, Minute, Second],[]} = io_lib:fread("~d-~d-~d_~d:~d:~d", String),
    {{Year, Month, Day}, {Hour, Minute, Second}}.

% Sends a response on its way in JSON
ret_json(Response, Req, State) ->
    {ok, R} = json:encode(Response),
    {ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"application/json">>}], R, Req),
    {ok, Req2, State}.


