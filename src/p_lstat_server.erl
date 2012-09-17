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

-module(p_lstat_server).
-behaviour(gen_server).
-define(SERVER, ?MODULE).

%% ------------------------------------------------------------------
%% API Function Exports
%% ------------------------------------------------------------------

-export([start_link/1, add_site/1, get_site/1]).

%% ------------------------------------------------------------------
%% gen_server Function Exports
%% ------------------------------------------------------------------

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-record(state, {site}).

%% ------------------------------------------------------------------
%% API Function Definitions
%% ------------------------------------------------------------------

start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Args], []).

add_site(Site) ->
    case get_site_server(Site) of
        {ok, Pid} ->
            {ok, Pid};
        {error, not_defined} ->
            pg2:create({?MODULE, site, Site}),
            supervisor:start_child(p_lstat_server_sup, [{site, Site}])
    end.

get_site(Site) ->
    get_site_server(Site).

%% ------------------------------------------------------------------
%% gen_server Function Definitions
%% ------------------------------------------------------------------

init([{site, Site}]) ->
    register_site(Site, self()),
    {ok, #state{site=Site}}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) ->
    {noreply, State}.

handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%% ------------------------------------------------------------------
%% Internal Function Definitions
%% ------------------------------------------------------------------
get_site_server(Name) ->
    case pg2:get_members({?MODULE, site, Name}) of
        [Pid] ->
            {ok, Pid};
        [] ->
            {error, died};
        {error, _Reason} ->
            {error, not_defined}
    end.

register_site(Site, Pid) ->
    pg2:join({?MODULE, site, Site}, Pid).
