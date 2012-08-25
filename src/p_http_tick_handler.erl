-module(p_http_tick_handler).
-behaviour(cowboy_http_handler).
-export([init/3, handle/2, terminate/2]).

% Included so we can cache qs, since that can be quite expensive.
-include("deps/cowboy/include/http.hrl").

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

get_qs(#http_req{raw_qs=RawQs} = Req) ->
    case simple_cache:lookup({qs, RawQs}) of
        {error, missing} ->
            {Qs, Req2} = cowboy_http_req:qs_vals(Req),
            simple_cache:set({qs, RawQs}, {Qs, Req2#http_req.qs_vals}, 60),
            {Qs, Req2};

        {ok, {Qs, AllQs}} ->
            Req2 = Req#http_req{qs_vals=AllQs},
            {Qs, Req2}
    end.

handle(Req, State) ->
    {Url, Req2} = get_referrer(Req),
    {Metrics, Req3} = get_qs(Req2),
    {Host, Req4} = cowboy_http_req:binding(host, Req3),
    case p_site_server:add_metrics(Host, [{<<"url">>, Url}| Metrics]) of
        {error, not_defined} ->
            {ok, FinalReq} = cowboy_http_req:reply(401, [], <<"Site Not Watched">>, Req4);
        ok ->
            Headers = [{'Content-Type', <<"text/plain">>}],
            {ok, FinalReq} = cowboy_http_req:reply(200, Headers, Req4)
    end,
            
    {ok, FinalReq, State}.

terminate(_Req, _State) ->
    ok.
