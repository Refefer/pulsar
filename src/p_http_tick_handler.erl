-module(p_http_tick_handler).
-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

get_ip_address(Req) ->
    {{N1,N2,N3,N4}, Req2} = cowboy_http_req:peer_addr(Req),
    {io_lib:format("~B.~B.~B.~B", [N1, N2, N3, N4]), Req2}.

parse_path(<<"http://", Rest/binary>>) ->
    remove_host(Rest);
parse_path(<<"https://", Rest/binary>>) ->
    remove_host(Rest);
parse_path(_Unknown) ->
    <<"Unknown Protocol">>.

remove_host(Path) ->
    case binary:split(Path, <<"/">>) of
        [Host, Rest] ->
            << <<"/">>/binary, Rest/binary>>;
        [Rest] ->
            % Not sure how this could happen since it's a bad url
            Rest
    end.
    
get_referrer(Req) ->
    case cowboy_http_req:header('Referer', Req, none) of
        {none, Req2} ->
            {<<"none">>, Req2};
        {Referrer, Req2} ->
            {parse_path(Referrer), Req2}
    end.

get_qs(Req) ->
    lists:foldl(fun(Q, {Vs, R}) ->
        {Value, R2} = cowboy_http_req:qs_val(Q, R, <<"none">>),
        {[Value | Vs], R2}
    end, {[], Req}, [<<"r">>]).

handle(Req, State) ->
    {Url, Req2} = get_referrer(Req),
    {[UrlRef], Req3} = get_qs(Req2),
    {Host, Req4} = cowboy_http_req:binding(host, Req3),
    case p_site_server:add_url(Host, {Url, UrlRef}) of
        {error, not_defined} ->
            {ok, FinalReq} = cowboy_http_req:reply(401, [], <<"Site Not Watched">>, Req4);
        ok ->
            Headers = [{'Content-Type', <<"text/plain">>}],
            {ok, FinalReq} = cowboy_http_req:reply(200, Headers, Req4)
    end,
            
    {ok, FinalReq, State}.

terminate(_Req, _State) ->
    ok.
