-module(gr_http_handler).
-export([init/3, handle/2, terminate/2]).

init({tcp, http}, Req, _Opts) ->
    {ok, Req, undefined_state}.

get_ip_address(Req) ->
    {{N1,N2,N3,N4}, _Req} = cowboy_http_req:peer_addr(Req),
    io_lib:format("~B.~B.~B.~B", [N1, N2, N3, N4]).

get_referrer(Req) ->
    case cowboy_http_req:header(referer, Req) of
        {undefined, Req} ->
            <<"undefined">>;
        {Value, _Req} ->
            Value
    end.

handle(Req, State) ->
    Peer = get_ip_address(Req),
    %{ok, Req2} = cowboy_http_req:reply(200, [], <<"Hello World!">>, Req),
    {ok, Req2} = cowboy_http_req:reply(200, [], [<<"Hello ">>, Peer, <<" from:">>, get_referrer(Req)], Req),
    {ok, Req2, State}.

terminate(_Req, _State) ->
    ok.
