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
         apply_group_modifiers/2,
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

% Crosstab all metrics against each other
cross_tab_metrics(all, Metrics) ->
    [{{Key1, Val1, Key2}, Val2} || {Key1, Val1} <- Metrics, 
                                   {Key2, Val2} <- Metrics, 
                                   Key1 =/= Key2];

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

sort_key_values(Values) ->
    lists:sort(fun({_K1, V}, {_K2, V2}) ->
        V >= V2
    end, Values).

% Filters data based on arguments passed in, respecting order.
apply_query_filters(Values, Req) ->
    {Qs, Req2} = p_http_utils:get_qs(Req),
    apply_filters(Qs, sort_key_values(Values), Req2).

apply_filters([], Values, _Req) ->
    Values;
% Takes the first Amount items from the list
apply_filters([{<<"_limit">>, Amount} | Rest], Values, Req) ->
    Num = list_to_integer(binary_to_list(Amount)),
    apply_filters(Rest, lists:sublist(Values, Num), Req);
% Reverses the list
apply_filters([{<<"_reverse">>, _Dir} | Rest], Values, Req) ->
    apply_filters(Rest, lists:reverse(Values), Req);
% Minimum value to return
apply_filters([{<<"_min">>, Amount} | _Rest], Values, _Req) ->
    Num = list_to_integer(binary_to_list(Amount)),
    lists:filter(fun({_Key, Value}) ->
        Value >= Num
    end, Values);
apply_filters([_Unknown | Rest], Values, Req) ->
    apply_filters(Rest, Values, Req).

% We only have one group modifier right now, _aggregate
apply_group_modifiers(Items, Req) ->
    {Qs, Req2} = p_http_utils:get_qs(Req),
    apply_group_modifiers(Qs, Items, Req2).
apply_group_modifiers([], Values, _Req) ->
    Values;
% Filters out all items that are not value
apply_group_modifiers([{<<"_value">>, Value}|Rest], Items, Req) ->
    % Split out the values
    {Values, NotValues} = lists:partition(fun({K, _V}) ->
        K == <<"_value">>
    end, Rest),

    % Build a set of all _values
    Set = lists:foldl(fun({_K, V}, S) ->
        sets:add_element(V, S)
    end, sets:from_list([Value]), Values),

    % Filter the items
    NewItems = gb_trees_foldl(fun({Tmsp, KVs}, Tree) ->
        NewKVs = lists:filter(fun({K, _V}) ->
            sets:is_element(K, Set)
        end, KVs),
        gb_trees:insert(Tmsp, NewKVs, Tree)
    end, gb_trees:empty(), Items),
    apply_group_modifiers(NotValues, NewItems, Req);
    
% Aggregates all the values across all timestamps
apply_group_modifiers([{<<"_aggregate">>, AggType}|Rest], Items, Req) ->
    NewItems = aggregate_stats(AggType, Items),
    apply_group_modifiers(Rest, NewItems, Req);

% Converts all timestamps from absolute time to deltas, in seconds
apply_group_modifiers([{<<"_timestamp">>, <<"delta">>}|Rest], Values, Req) ->
    Now = calendar:datetime_to_gregorian_seconds(erlang:localtime()),
    Tree = gb_trees_foldl(fun({Tmsp, KVs}, Acc) ->
       Key = if
            is_tuple(Tmsp) ->
                Delta = Now - calendar:datetime_to_gregorian_seconds(Tmsp),
                list_to_binary(integer_to_list(Delta));
            true ->
                Tmsp
        end,
        gb_trees:insert(Key, KVs, Acc)
    end, gb_trees:empty(), Values),
    apply_group_modifiers(Rest, Tree, Req);
apply_group_modifiers([_Other|Rest], Values, Req) ->
    apply_group_modifiers(Rest, Values, Req).

get_agg_key(<<"min">>, {{Year, Month, Day}, {Hour, Min, _Sec}}) ->
    {{Year, Month, Day}, {Hour, Min, 0}};
get_agg_key(<<"hour">>, {{Year, Month, Day}, {Hour, _Min, _Sec}}) ->
    {{Year, Month, Day}, {Hour, 0, 0}};
get_agg_key(<<"day">>, {{Year, Month, Day}, {_Hour, _Min, _Sec}}) ->
    {{Year, Month, Day}, {0, 0, 0}};
get_agg_key(<<"all">>, _Date) ->
    <<"all">>;
get_agg_key(_Unknown, Date) ->
    Date.

% Buckets and aggregates metrics based on their timestamps
aggregate_stats(Type, Tree) ->
    aggregate_stats(Type, gb_trees:keys(Tree), Tree, gb_trees:empty()).
aggregate_stats(_Type, [], _Tree, Agg) ->
    Agg;
aggregate_stats(Type, Tmsps, Tree, AggTree) ->
    [Tmsp|_Rest] = Tmsps,
    AggKey = get_agg_key(Type, Tmsp),
    {TmspGroup, Rest} = lists:splitwith(fun(D) -> get_agg_key(Type, D) == AggKey end, Tmsps),
    D = lists:foldl(fun(T, Dict) ->
        {value, KVList} = gb_trees:lookup(T, Tree),
        lists:foldl(fun({Key, Value}, D) ->
            dict:update_counter(Key, Value, D)
        end, Dict, KVList)
    end, dict:new(), TmspGroup),
    NewAgg = gb_trees:insert(AggKey, dict:to_list(D), AggTree),
    aggregate_stats(Type, Rest, Tree, NewAgg).
    
% Converts an erlang time to string
time_to_string({{Year, Month, Day},{Hour, Minute, Second}}) ->
    io_lib:format("~p-~p-~p_~p:~p:~p", [Year, Month, Day, Hour, Minute, Second]).
% Do we have a negative delta?
string_to_time(<<"-", Minutes/binary>>) ->
    Mins = list_to_integer(binary_to_list(Minutes)),
    Delta = calendar:datetime_to_gregorian_seconds(erlang:localtime()) - Mins*60,
    calendar:gregorian_seconds_to_datetime(Delta);
string_to_time(Binary) ->
    String = binary_to_list(Binary),
    {ok, [Year, Month, Day, Hour, Minute, Second],[]} = io_lib:fread("~d-~d-~d_~d:~d:~d", String),
    {{Year, Month, Day}, {Hour, Minute, Second}}.

% Sends a response on its way in JSON
ret_json(Response, Req, State) ->
    {ok, R} = json:encode(Response),
    {ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"application/json">>}], R, Req),
    {ok, Req2, State}.

% foldl function for gb_trees that uses iterators
gb_trees_foldl(_Fun, Acc, none) ->
    Acc;
gb_trees_foldl(Fun, Acc, {Key, Value, Iter}) ->
    gb_trees_foldl(Fun, Fun({Key, Value}, Acc), gb_trees:next(Iter));
gb_trees_foldl(Fun, Acc, Tree) ->
    gb_trees_foldl(Fun, Acc, gb_trees:next(gb_trees:iterator(Tree))).
