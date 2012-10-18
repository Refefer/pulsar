-module(pulsar_header).
-export([path_to_hostname/2, through/2, url_to_path/2]).

% Maps the header straight through
through({Header, missing}, Opts) ->
    return_default(Header, Opts);
through({Header, RawValue}, Opts) ->
    Stat = get_stat_name(Opts, Header),
    [{Stat, RawValue}].

url_to_path({Header, missing}, Opts) ->
    return_default(Header, Opts);
url_to_path({Header, RawValue}, Opts) ->
    Stat = get_stat_name(Opts, Header),
    [{Stat, parse_path(RawValue)}].

% Takes a url path and converts it to its hostname
path_to_hostname({Header, RawHeader}, Opts) ->
    Stat = get_stat_name(Opts, Header),
    case RawHeader of
        missing ->
            return_default(Header, Opts);
        RawHeader ->
            [{Stat, parse_path(RawHeader)}]
    end.

get_stat_name(Opts, Header) ->
    case proplists:get_value(name, Opts, Header) of
        Header ->
            atom_to_binary(Header, latin1);
        OtherName ->
            list_to_binary(OtherName)
    end.

return_default(Header, Opts) ->
    Stat = get_stat_name(Opts, Header),
    case proplists:get_value(missing_value, Opts, undefined) of
        undefined ->
            [];
        MissingValue ->
            [{Stat, MissingValue}]
    end.


% Attempts to remove everything except the path from a Referer. 
parse_path(<<"http://", Rest/binary>>) ->
    remove_host(Rest);
parse_path(<<"https://", Rest/binary>>) ->
    remove_host(Rest);
parse_path(_Unknown) ->
    <<"Unknown Protocol">>.

remove_host(Path) ->
    case binary:split(Path, <<"/">>) of
        [_Host, Rest] ->
            << "/", Rest/binary>>;
        [Rest] ->
            % Not sure how this could happen since it's a bad url
            Rest
    end.

