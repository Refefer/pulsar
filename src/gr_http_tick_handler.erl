-module(gr_http_tick_handler).
-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

get_ip_address(Req) ->
    {{N1,N2,N3,N4}, Req2} = cowboy_http_req:peer_addr(Req),
    {io_lib:format("~B.~B.~B.~B", [N1, N2, N3, N4]), Req2}.

get_referrer(Req) ->
    cowboy_http_req:header('Referer', Req, <<"none">>).

get_qs(Req) ->
    lists:foldl(fun(Q, {Vs, R}) ->
        {Value, R2} = cowboy_http_req:qs_val(Q, R, <<"none">>),
        {[Value | Vs], R2}
    end, {[], Req}, [<<"r">>]).

handle(Req, State) ->
    {Url, Req2} = get_referrer(Req),
    {[UrlRef], Req3} = get_qs(Req2),
    {Host, Req4} = cowboy_http_req:binding(host, Req3),
    case gr_site_server:add_url(Host, {Url, UrlRef}) of
        {error, not_defined} ->
            {ok, FinalReq} = cowboy_http_req:reply(401, [], <<"Site Not Watched">>, Req4);
        ok ->
            Headers = [{'Content-Type', <<"text/plain">>}],
            {ok, FinalReq} = cowboy_http_req:reply(200, Headers, Req4)
    end,
            
    {ok, FinalReq, State}.

terminate(_Req, _State) ->
    ok.
