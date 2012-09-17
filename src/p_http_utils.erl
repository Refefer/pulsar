-module(p_http_utils).
-export([get_ip_address/1,
        remove_host/1,
        get_referrer/1,
        get_referrer/2,
        get_qs/1,
        parse_request/1]).
        

% Included so we can cache qs, since that can be quite expensive.
-include("deps/cowboy/include/http.hrl").

% Pulls the ip address of the connected client off of the request.
get_ip_address(Req) ->
    {{N1,N2,N3,N4}, Req2} = cowboy_http_req:peer_addr(Req),
    {io_lib:format("~B.~B.~B.~B", [N1, N2, N3, N4]), Req2}.

% Attempts to remove everything except the path from a Referer. 
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
    
% Grabs the Referer header from the request
get_referrer(Req) ->
    get_referrer(Req, <<"none">>).
get_referrer(Req, Default) ->
    case cowboy_http_req:header('Referer', Req, none) of
        {none, Req2} ->
            {Default, Req2};
        {Referrer, Req2} ->
            {parse_path(Referrer), Req2}
    end.

% Gets the query values from the path, looking up and caching them.
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

% Parses out the useful information from a Request object.
parse_request(Req) ->
    {Url, Req2} = get_referrer(Req),
    {Metrics, Req3} = get_qs(Req2),
    {Host, Req4} = cowboy_http_req:binding(host, Req3),
    AllMetrics = [{<<"stats">>, <<"total">>}, {<<"url">>, Url} | Metrics],
    {Host, AllMetrics, Req4}.
