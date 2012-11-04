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
            {Key, Req3} = cowboy_http_req:binding(key, Req2),
            Values = get_values(Host, Key),
            ret_json(Values, Req3, State)
    end.

terminate(_Req, _State) ->
    ok.

watch(Host, Key, Req) ->
    {QKey, Req2} = get_key_filter(Key, Req),
    case pulsar_stat:subscribe(Host, self()) of
        {error, _Reason} ->
            ret(501, Req2, [], <<"">>);
        ok ->
            Headers = [{'Content-Type', <<"text/event-stream">>}],
            {ok, Req3} = cowboy_http_req:chunked_reply(200, Headers, Req2),
            watch_loop(Req3, {site, Host, [QKey]})
    end.

with_host_q(Fun, Req, State) ->
    case cowboy_http_req:qs_val(<<"host">>, Req, undefined) of
        {undefined, Req2} ->
            ret_404(Req2, State);
        {Host, Req2} ->
            Fun(Host, Req2)
    end.

take_qs(Keys, Qs) ->
    take_qs(Keys, Qs, []).
take_qs([], _Qs, Acc) -> 
    lists:reverse(Acc);
take_qs([Key|Rest], Qs, Acc) -> 
    case lists:keytake(Key, 1, Qs) of
        false ->
            take_qs(Rest, Qs, [undefined|Acc]);
        {value, {Key, Value}, RestQs} ->
            take_qs(Rest, RestQs, [Value|Acc])
    end.

% Grabs the key filters off of the url query 
get_key_filter(Key, Req) ->
    {Qs, Req2} = p_http_utils:get_qs(Req),
    case take_qs([<<"value">>, <<"subkey">>], Qs) of
        [undefined, _SubKey] ->
            {Key, Req2};
        [_Value, undefined] ->
            {Key, Req2};
        [KeyValue, SubKey] ->
            {{Key, KeyValue, SubKey}, Req2}
    end.

ret_200(Req, State) ->
    ret(200, Req, State, <<"">>).

ret_json(Response, Req, State) ->
    {ok, R} = json:encode(Response),
    {ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"application/json">>}], R, Req),
    {ok, Req2, State}.

ret_404(Req, State) ->
    ret(404, Req, State, <<"">>).

ret(Code, Req, State, Msg) ->
    {ok, Req2} = cowboy_http_req:reply(Code, [], [Msg], Req),
    {ok, Req2, State}.
            
get_values(Server, Key) ->
    % From perm table
    Dict = p_history_server:get_key(Server, Key),
    lists:map(fun({K,V}) -> [K,V] end, dict:to_list(Dict)).
    
format_key({Key, Value, Subkey}) ->
    io_lib:format("~s:~s:~s", [Key, Value, Subkey]);
format_key(Key) ->
    Key.

send_all_data(_Server, _Time, [], _Req) ->
    ok;
send_all_data(Server, Time, [Key|Rest], Req) ->
    Values = get_values(Server, Key),
    Event = [<<"event: ">>, format_key(Key), <<"\n">>,
             <<"time: ">>, time_to_string(Time), <<"\n">>,
             <<"data: ">>, json:encode(Values), <<"\n">>,
             <<"\n">>],
    case cowboy_http_req:chunk(Event, Req) of
        ok -> 
            send_all_data(Server, Time, Rest, Req);
        {error, closed} ->
            % We are done here!
            closed
    end.

watch_loop(Req, State={site, Host, Keys}) ->
    receive
        {published, SPid, Time, Host} ->
            case send_all_data(SPid, Time, Keys, Req) of
                ok ->
                    watch_loop(Req, State);
                closed ->
                    {ok, Req, undefined}
            end;
        {p_stat_server, {terminate, Host}} ->
            % Someone said to stop watching the site, so exit.
            {ok, Req, finished}
    end.

time_to_string({{Year, Month, Day},{Hour, Minute, Second}}) ->
    io_lib:format("~p-~p-~p ~p:~p:~p", [Year, Month, Day, Hour, Minute, Second]).