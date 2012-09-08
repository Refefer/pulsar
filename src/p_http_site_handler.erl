-module(p_http_site_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    case cowboy_http_req:binding(command, Req) of
        {undefined, Reqs} ->
            {ok, Req3} = cowboy_http_req:reply(404, [], [<<"">>]),
            {ok, Req3, State};
        {Command, Req2} ->
            handle({command, Command}, Req, State)
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
    {ok, R} = json:encode(p_stat_server:list_sites()),
    {ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"application/json">>}], R, Req),
    {ok, Req2, State};

handle({command, <<"watch">>}, Req, State) ->
    with_host_q(fun(Site, Req2) ->
        case cowboy_http_req:qs_val(<<"key">>, Req2, undefined) of
            {undefined, Req2} ->
                ret(415, Req, State, <<"Missing 'key' field">>);
            {Keys, Req2} ->
                case p_stat_server:add_site_listener(Site, self()) of
                    {error, Reason} ->
                        ret(501, Req2, State, <<"">>);
                    ok ->
                        Headers = [{'Content-Type', <<"text/event-stream">>}],
                        {ok, Req3} = cowboy_http_req:chunked_reply(200, Headers, Req2),
                        watch_loop(Req3, {site, Site, binary:split(Keys, <<",">>, [global])})
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

ret_200(Req, State) ->
    ret(200, Req, State, <<"">>).

ret_404(Req, State) ->
    ret(404, Req, State, <<"">>).
ret(Code, Req, State, Msg) ->
    {ok, Req2} = cowboy_http_req:reply(Code, [], [Msg], Req),
    {ok, Req2, State}.
            
build_data(Table) ->
    build_data(Table, <<"url">>).

build_data(Table, Type) ->
    {ok, Matches} = json:encode(ets:match(Table, {{Type, '$1'}, '$2'})),
    [<<"data:">>, Matches, <<"\n">>].

send_all_data(_Table, _Req, []) ->
    ok;
send_all_data(Table, Req, [Key|Rest]) ->
    Event = [<<"event: ">>, Key, <<"\n">>,
             build_data(Table, Key), 
             <<"\n">>],
    case cowboy_http_req:chunk(Event, Req) of
        ok -> 
            send_all_data(Table, Req, Rest);
        {error, closed} ->
            % We are done here!
            closed
    end.

watch_loop(Req, State={site, Site, Keys}) ->
    receive
        {p_stat_server, {stats, Time, Table}} ->
            case send_all_data(Table, Req, Keys) of
                ok ->
                    watch_loop(Req, State);
                closed ->
                        p_stat_server:remove_site_listener(Site, self()),
                        io:format("Client closed ~n", []),
                        {ok, Req, undefined}
            end;
        {p_stat_server, {terminate, Site}} ->
            % Someone said to stop watching the site, so exit.
            {ok, Req, finished}
    end.
