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

