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
    p_http_utils:ret_json(pulsar_stat:list_hosts(), Req, []);

handle(Req, [host|State]) ->
   {Host, Req2} = cowboy_http_req:binding(host, Req),
   case State of 
        [] ->
            p_http_utils:ret_json([Host, <<"Keys:">>], Req2, State);
        [key|_Rest]->
            {Key, Req3} = p_http_utils:get_query_key(Req2),
            Results = case get_time_range(Req3) of
                {nil, nil} ->
                    lookup_values(Host, Key, Req3);
                {Start, End} ->
                    lookup_values(Host, Key, Start, End, Req3)
            end,
            ModResults = p_http_utils:apply_group_modifiers(Results, Req3),
            FormattedResults = lists:map(fun({Tmsp, Values}) ->
                if
                    is_binary(Tmsp) ->
                        {Tmsp, {Values}};
                    is_tuple(Tmsp) ->
                        {p_http_utils:time_to_string(Tmsp), {Values}}
                end
            end, ModResults),
            p_http_utils:ret_json({lists:reverse(FormattedResults)}, Req3, State)
    end.

terminate(_Req, _State) ->
    ok.

lookup_values(Host, Key, Req) ->
    case pulsar_stat:query_host(Host, Key) of
        {error, _Reason} ->
            [];
        {Timestamp, Dict} ->
            filter_tree(gb_trees:insert(Timestamp, dict:to_list(Dict), gb_trees:empty()), Req)
    end.

lookup_values(Host, Key, Start, End, Req) ->
    case pulsar_stat:query_history(Host, Key, Start, End) of
        {error, Reason} ->
            [];
        {ok, Tree} ->
            filter_tree(Tree, Req)
    end.

get_time_range(Req) ->
    {Qs, Req2} = p_http_utils:get_qs(Req),
    case proplists:get_value(<<"_start">>, Qs) of
        undefined ->
            {nil, nil};
        STime ->
            Start = p_http_utils:string_to_time(binary_to_list(STime)),
            End = case proplists:get_value(<<"_end">>, Qs) of
                undefined ->
                    erlang:localtime();
                ETime ->
                    p_http_utils:string_to_time(binary_to_list(ETime))
            end,
            {Start, End}
    end.

% runs filter commands against a range of data
filter_tree(Tree, Req) ->
    filter_tree(gb_trees:next(gb_trees:iterator(Tree)), Req, []).
filter_tree(none, Req, Acc) ->
    Acc;
filter_tree({Tmsp, Values, Iter}, Req, Acc) ->
    FilteredValues = p_http_utils:apply_query_filters(Values, Req),
    filter_tree(gb_trees:next(Iter), Req, [{Tmsp, FilteredValues}|Acc]).
