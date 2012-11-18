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

-module(p_http_stats_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, Opts) ->
    {ok, Req, Opts}.

% List hosts
handle(Req, []) ->
    ret_json(pulsar_stat:list_hosts(), Req, []);

handle(Req, [host|State]) ->
   {Host, Req2} = cowboy_http_req:binding(host, Req),
   case State of 
        [] ->
            ret_json([Host, <<"Keys:">>], Req2, State);
        [key|_Rest]->
            {Key, Req3} = get_key(Req2),
            Values = lookup_values(Host, Key, Req),
            ret_json(Values, Req3, State)
    end.

terminate(_Req, _State) ->
    ok.

lookup_values(Host, Key, Req) ->
    CacheKey = {?MODULE, Host, Key},
    Values = case simple_cache:lookup(CacheKey) of
        {ok, V} ->
            V;
        {error, missing} ->
            V = lists:sort(fun([_V, Num1],[_V2, Num2]) -> 
                Num2 =< Num1
            end, get_values(Host, Key)),
            simple_cache:set(CacheKey, V, 5),
            V
    end,
    apply_filters(Values, Req).

get_key(Req) ->
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
apply_filters(Values, Req) ->
    {Qs, Req2} = p_http_utils:get_qs(Req),
    io:format("QS: ~p~n", [Qs]),
    apply_filters(Qs, Values, Req2).

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
    lists:filter(fun([Key, _V]) ->
        Key == DKey
    end, Values);
apply_filters([_Unknown | Rest], Values, Req) ->
    apply_filters(Rest, Values, Req).

ret_json(Response, Req, State) ->
    {ok, R} = json:encode(Response),
    {ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"application/json">>}], R, Req),
    {ok, Req2, State}.

ret(Code, Req, State, Msg) ->
    {ok, Req2} = cowboy_http_req:reply(Code, [], [Msg], Req),
    {ok, Req2, State}.
            
get_values(Host, Timestamp, Key) ->
    % From perm table
    Dict = pulsar_stat:query_host(Host, Timestamp, Key),
    lists:map(fun({K,V}) -> [K,V] end, dict:to_list(Dict)).
    
get_values(Host, Key) ->
    % From perm table
    {Timestamp, Dict} = pulsar_stat:query_host(Host, Key),
    lists:map(fun({K,V}) -> [K,V] end, dict:to_list(Dict)).
    
time_to_string({{Year, Month, Day},{Hour, Minute, Second}}) ->
    io_lib:format("~p-~p-~p ~p:~p:~p", [Year, Month, Day, Hour, Minute, Second]).
