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

-module(p_site_server_test).
-include_lib("eunit/include/eunit.hrl").

-define(SITE, foo).
-define(URLS, [{1,2}, {2,3}, {1,4}, {2,5}, {3,6}]).

setup_site(Site) ->
    % make sure the supervisor is started
    p_site_server_sup:start_link(),
    p_site_server:add_site(Site).

add_listener(Site) ->
    p_site_server:add_site_listener(Site, self()).

add_urls(Site) ->
    add_urls(Site, ?URLS).

add_urls(Site, Urls) ->
    lists:foreach(fun(UR) ->
        p_site_server:add_url(Site, UR)
    end, Urls).

simple_test() ->
    setup_site(?SITE),
    add_listener(?SITE),
    add_urls(?SITE),
    receive 
        {urls, _Time, Urls} ->
            ?assert(lists:sort(?URLS) =:= Urls)
    after
        1100 ->
            ?assert(false)
    end.

listen_loop(0, _Other) ->
    p_site_server:remove_site_listener(self());

listen_loop(N, [Exp|Rest]) ->
    receive 
        {urls, Time, Urls} ->
            ?debugFmt("Received message ~p at ~p~n", [Time, Urls]),
            ?assert(Urls =:= Exp),
            listen_loop(N-1, Rest)
    after
        1100 ->
            ?assert(fail)
    end.

spawn_listen_loops(0, _Site, _NumLoops, _Equals) ->
    ok;
spawn_listen_loops(Num, Site, NumLoops, Equals) ->
    spawn(fun() -> 
        p_site_server:add_site_listener(Site, self()),
        listen_loop(2, Equals) 
    end),
    spawn_listen_loops(Num-1, Site, NumLoops, Equals).

multi_listener_test() ->
    setup_site(?SITE),
    SUrls = lists:sort(?URLS),
    spawn_listen_loops(5, ?SITE, 2, [SUrls, SUrls]),
    add_urls(?SITE),
    timer:sleep(1000),
    add_urls(?SITE).
