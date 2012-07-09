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
        p_site_server:add_site(Host),
        ret_200(Req2, State)
    end, Req, State);

handle({command, <<"remove">>}, Req, State) ->
    with_host_q(fun(Host, Req2) ->
        p_site_server:delete_site(Host),
        ret_200(Req2, State)
    end, Req, State);

handle({command, <<"list">>}, Req, State) ->
    R = bracket((fun() ->
        Response = lists:map(fun(Site) ->
            [<<",">>, quote(Site)]
        end, p_site_server:list_sites()),
        case Response of 
            [[Q, Site] | Sites] ->
                [Site | Sites];
            [] ->
                []
        end
    end)()),
    {ok, Req2} = cowboy_http_req:reply(200, [{'Content-Type', <<"application/json">>}], R, Req),
    {ok, Req2, State};

handle({command, <<"watch">>}, Req, State) ->
    with_host_q(fun(Site, Req2) ->
        case p_site_server:add_site_listener(Site, self()) of
            {error, _Reason} ->
                ret(501, Req2, State, <<"">>);
            ok ->
                Headers = [{'Content-Type', <<"text/event-stream">>}],
                {ok, Req3} = cowboy_http_req:chunked_reply(200, Headers, Req2),
                handle_loop(Req3, {site, Site})
        end
    end, Req, State);

handle({command, _Unknown}, Req, State) ->
    ret_404(Req, State).

with_host_q(Fun, Req, State) ->
    case cowboy_http_req:qs_val(<<"host">>, Req, undefined) of
        {undefined, Req2} ->
            ret_404(Req2, State);
        {Host, Req2} ->
            Fun(Host,Req2)
    end.

ret_200(Req, State) ->
    ret(200, Req, State, <<"">>).

ret_404(Req, State) ->
    ret(404, Req, State, <<"">>).
ret(Code, Req, State, Msg) ->
    {ok, Req2} = cowboy_http_req:reply(Code, [], [Msg], Req),
    {ok, Req2, State}.
            
wrap(Item, Char) ->
    wrap(Item, Char, Char).
wrap(Item, SChar, EChar) ->
    [SChar, Item, EChar].

bracket(Item) ->
    wrap(Item, <<"[">>, <<"]">>).

quote(Item) ->
    wrap(Item, <<"\"">>).

urls_to_binary(Urls) ->
    bracket(urls_to_binary(lists:reverse(Urls), [])).
urls_to_binary([], [Comma|Acc]) ->
    Acc;
urls_to_binary([], []) ->
    [];
urls_to_binary([{{Url, Ref}, Count}|Rest], Acc) ->
    S = bracket([quote(Url), ",", quote(Ref), ",", integer_to_list(Count)]),
    urls_to_binary(Rest, [<<",">>,S|Acc]).

build_data(Urls) ->
    [<<"data:">>, urls_to_binary(ets:match_object(Urls, '_')), <<"\n">>].

handle_loop(Req, State={site, Site}) ->
    receive
        {p_site_server, {urls, Time, Urls}} ->
            Event = [<<"event: urls\n">>, 
                     build_data(Urls), 
                     <<"\n">>],
            case cowboy_http_req:chunk(Event, Req) of
                ok -> 
                    handle_loop(Req, State);
                {error, closed} ->
                    % We are done here!
                    p_site_server:remove_site_listener(Site, self()),
                    {ok, Req, undefined}
            end;

        {p_site_server, {terminate, Site}} ->
            % Someone said to stop watching the site, so exit.
            {ok, Req, finished}
    end.
