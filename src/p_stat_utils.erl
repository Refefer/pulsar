-module(p_stat_utils).

get_listeners(Site) ->
    pg2:get_members({?MODULE, listener, Site}).

register_listener(Site, Pid) ->
    pg2:join({?MODULE, listener, Site}, Pid).

unregister_listener(Site, Pid) ->
    pg2:leave({?MODULE, listener, Site}, Pid).

register_site(Site, Pid) ->
    pg2:join({?MODULE, site, Site}, Pid).

