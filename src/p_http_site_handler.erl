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

-module(p_http_site_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    case cowboy_http_req:binding(command, Req) of
        {undefined, _Reqs} ->
            {ok, Req3} = cowboy_http_req:reply(404, [], [<<"">>]),
            {ok, Req3, State};
        {Command, Req2} ->
            handle({command, Command}, Req2, State)
   end.


terminate(_Req, _State) ->
    ok.

% Internal functions
handle({command, <<"add">>}, Req, State) ->
    with_host_q(fun(Host, Req2) ->
        p_stat_server:add_site(Host),
        ret_200(Req2, State)
    end, Req, State);

handle({command, <<"remove">>}, Req, State) ->
    with_host_q(fun(Host, Req2) ->
        p_stat_server:delete_site(Host),
        ret_200(Req2, State)
    end, Req, State);

handle({command, <<"list">>}, Req, State) ->
    {ok, R} = json:encode(pulsar_stat:list_sites()),
    {ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"application/json">>}], R, Req),
    {ok, Req2, State};

handle({command, <<"watch">>}, Req, State) ->
    with_host_q(fun(Site, Req2) ->
        case get_key(Req2) of 
            {undefined, Req2} ->
                ret(415, Req, State, <<"Missing 'key' field">>);
            {Key, Req2} ->
                case pulsar_stat:subscribe(Site, self()) of
                    {error, _Reason} ->
                        ret(501, Req2, State, <<"">>);
                    ok ->
                        Headers = [{'Content-Type', <<"text/event-stream">>}],
                        {ok, Req3} = cowboy_http_req:chunked_reply(200, Headers, Req2),
                        watch_loop(Req3, {site, Site, [Key]})
                end
        end
    end, Req, State);

handle({command, _Unknown}, Req, State) ->
    ret_404(Req, State).

with_host_q(Fun, Req, State) ->
    case cowboy_http_req:qs_val(<<"host">>, Req, undefined) of
        {undefined, Req2} ->
            ret_404(Req2, State);
        {Host, Req2} ->
            Fun(Host, Req2)
    end.

take_qs(Keys, Qs) ->
    take_qs(Keys, Qs, []).

take_qs([], _Qs, Acc) -> lists:reverse(Acc);
take_qs([Key|Rest], Qs, Acc) -> 
    case lists:keytake(Key, 1, Qs) of
        false ->
            take_qs(Rest, Qs, [undefined|Acc]);
        {value, {Key, Value}, RestQs} ->
            take_qs(Rest, RestQs, [Value|Acc])
    end.

% Grabs a key or composite key from query strings. 
get_key(Req) ->
    {Qs, Req2} = p_http_utils:get_qs(Req),
    case take_qs([<<"key">>, <<"value">>, <<"subkey">>], Qs) of
        [undefined, _Value, _SubKey] ->
            {undefined, Req2};
        [Key, undefined, _SubKey] ->
            {Key, Req2};
        [Key, _Value, undefined] ->
            {Key, Req2};
        [Key, KeyValue, SubKey] ->
            {{Key, KeyValue, SubKey}, Req2}
    end.

ret_200(Req, State) ->
    ret(200, Req, State, <<"">>).

ret_404(Req, State) ->
    ret(404, Req, State, <<"">>).

ret(Code, Req, State, Msg) ->
    {ok, Req2} = cowboy_http_req:reply(Code, [], [Msg], Req),
    {ok, Req2, State}.
            
build_data(Server, Time, Key) ->
    % From perm table
    Dict = p_history_server:get_key(Server, Time, Key),

    KVList = lists:map(fun({K,V}) -> [K,V] end, dict:to_list(Dict)),
    {ok, Matches} = json:encode(KVList),
    [<<"data:">>, Matches, <<"\n">>].

format_key({Key, Value, Subkey}) ->
    io_lib:format("~s:~s:~s", [Key, Value, Subkey]);
format_key(Key) ->
    Key.

send_all_data(_Server, _Time, [], _Req) ->
    ok;
send_all_data(Server, Time, [Key|Rest], Req) ->
    Event = [<<"event: ">>, format_key(Key), <<"\n">>,
             <<"time: ">>, time_to_string(Time), <<"\n">>,
             build_data(Server, Time, Key), 
             <<"\n">>],
    case cowboy_http_req:chunk(Event, Req) of
        ok -> 
            send_all_data(Server, Time, Rest, Req);
        {error, closed} ->
            % We are done here!
            closed
    end.

watch_loop(Req, State={site, Site, Keys}) ->
    receive
        {published, SPid, Time, Site} ->
            case send_all_data(SPid, Time, Keys, Req) of
                ok ->
                    watch_loop(Req, State);
                closed ->
                        %p_stat_server:remove_site_listener(Site, self()),
                        io:format("Client closed ~n", []),
                        {ok, Req, undefined}
            end;
        {p_stat_server, {terminate, Site}} ->
            % Someone said to stop watching the site, so exit.
            {ok, Req, finished}
    end.

time_to_string({{Year, Month, Day},{Hour, Minute, Second}}) ->
    io_lib:format("~p-~p-~p ~p:~p:~p", [Year, Month, Day, Hour, Minute, Second]).
