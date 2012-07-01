-module(gr_http_watch_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

init({_Any, http}, Req, []) ->
    {ok, Req, undefined}.

handle(Req, State) ->
    case cowboy_http_req:binding(host, Req) of 
        {undefined, Req2} ->
            {ok, Req3} = cowboy_http_req:reply(404, [], [<<"">>]),
            {ok, Req3, State};

        {Site, Req2} ->
            case gr_site_server:add_site_listener(Site, self()) of
                {error, _Reason} ->
                    {ok, Req3} = cowboy_http_req:reply(501, [], [<<"Site not tracked">>], Req2),
                    {ok, Req3, State};
                ok ->
                    Headers = [{'Content-Type', <<"text/event-stream">>}],
                    {ok, Req3} = cowboy_http_req:chunked_reply(200, Headers, Req2),
                    handle_loop(Req3, {site, Site})
            end
    end.

terminate(_Req, _State) ->
    ok.
            
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
urls_to_binary([], Acc) ->
    Acc;
urls_to_binary([{{Url, Ref}, Count}|Rest], Acc) ->
    S = bracket([quote(Url), ",", quote(Ref), ",", integer_to_list(Count)]),
    urls_to_binary(Rest, [S|Acc]).

build_data(Urls) ->
    [<<"data:">>, urls_to_binary(ets:match_object(Urls, '_')), <<"\n">>].

handle_loop(Req, State={site, Site}) ->
    receive
        {gr_site_server, {urls, Time, Urls}} ->
            Event = [<<"event: urls\n">>, 
                     build_data(Urls), 
                     <<"\n">>],
            case cowboy_http_req:chunk(Event, Req) of
                ok -> 
                    handle_loop(Req, State);
                {error, closed} ->
                    % We are done here!
                    gr_site_server:remove_site_listener(Site, self()),
                    {ok, Req, undefined}
            end;

        {gr_site_server, {terminate, Site}} ->
            % Someone said to stop watching the site, so exit.
            {ok, Req, finished}
    end.
